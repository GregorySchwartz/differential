{- Differential
Gregory W. Schwartz

Collects the functions pertaining to finding the differential between groups for
each entity.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Differential
    ( getDifferentials
    , differentialMatrixObsRow
    , differentialMatrixFeatRow
    , getDifferential
    , edgeR
    ) where

-- Standard
import           Control.Monad (guard)
import           Data.Int (Int32)
import           Data.List
import           Data.Semigroup
import           TextShow (showt)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Aeson.Lens as L
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified H.Prelude as H
import qualified Statistics.Test.KruskalWallis as Stat
import qualified Statistics.Types as Stat

-- Cabal

import           Language.R as R
import           Language.R.Instance as R
import           Language.R.Literal as R
import           Language.R.QQ

-- Local
import           Types
import           Utility

-- | Get unique pairings of a list. From
-- http://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- | Find the p-value of two samples.
differential :: [Double] -> [Double] -> R s (Maybe PValue)
differential xs ys = [r| suppressWarnings(wilcox.test(xs_hs, ys_hs))$p.value |]
                 >>= (\x -> return . Just . PValue $ ((R.fromSomeSEXP x) :: Double))

-- | Find the p-value of two samples using the Kruskal-Wallis test.
differentialKW :: [Double] -> [Double] -> Maybe PValue
differentialKW xs ys = do
  guard ((> 1) . Set.size . Set.fromList $ xs <> ys) -- Ensure not everything is the same rank
  res <- Stat.kruskalWallisTest [V.fromList xs, V.fromList ys]
  return . PValue . Stat.pValue . Stat.testSignificance $ res

-- | For two lists, xs and ys, find the log2 fold change as mean ys / mean xs.
getLog2Diff :: [Double] -> [Double] -> Log2Diff
getLog2Diff xs ys = Log2Diff . logBase 2 $ getMean ys / getMean xs
  where
    getMean = Fold.fold Fold.mean

-- | Get a comparison using the Kruskal-Wallis test.
getDifferential :: Status
                -> Status
                -> [Double]
                -> [Double]
                -> (Comparison, Log2Diff, Maybe PValue)
getDifferential (Status !s1) (Status !s2) !l1 !l2 = (comp, diff, pVal)
  where
    pVal = differentialKW l1 l2
    diff = getLog2Diff l1 l2
    comp = Comparison (s2 <> "/" <> s1)

-- | Get all comparisons of a Name.
getNameDifferentials :: Map.Map Status (Seq.Seq Double) -> ComparisonMap
getNameDifferentials m = ComparisonMap . Map.fromList . fmap comp $ comparisons
  where
    comparisons = pairs . Map.keys $ m
    comp (!s1, !s2) = (\(x, _, z) -> (x, z))
                    $ getDifferential
                        s1
                        s2
                        (F.toList . (Map.!) m $ s1)
                        (F.toList . (Map.!) m $ s2)

-- | Convert a ComparisonMap to an OutputMap.
comparisonMapToOutputMap :: ComparisonMap -> OutputMap
comparisonMapToOutputMap = OutputMap
                         . Map.map (maybe "NA" (showt . unPValue))
                         . Map.mapKeys unComparison
                         . unComparisonMap

-- | Get all p-values in all relevant combinations.
getDifferentials :: NameMap -> [(Name, OutputMap)]
getDifferentials (NameMap nameMap) =
  Map.elems
    . Map.mapWithKey (\ !k -> (k,)
                            . comparisonMapToOutputMap
                            . getNameDifferentials
                     )
    $ nameMap

-- | Get differentials between columns (features) of select rows (observations)
-- of bs / as, where as and bs are lists of row indices.
differentialMatrixObsRow :: [Int] -- ^ as
                         -> [Int] -- ^ bs
                         -> S.SpMatrix Double
                         -> [(Log2Diff, Maybe PValue, Maybe FDR)]
differentialMatrixObsRow as bs = differentialMatrixFeatRow as bs . S.transpose

-- | Get differentials between columns (observations) of select rows (features)
-- of bs / as, where as and bs are lists of row indices.
differentialMatrixFeatRow :: [Int] -- ^ as
                          -> [Int] -- ^ bs
                          -> S.SpMatrix Double
                          -> [(Log2Diff, Maybe PValue, Maybe FDR)]
differentialMatrixFeatRow as bs = withFDR . fmap obsToDiff . S.toRowsL
  where
    withFDR xs = zipWith (\(!l, !p) fdr -> (l, p, fdr)) xs
               . getFDR 0.05
               . fmap snd
               $ xs
    obsToDiff vec = (\(_, !l, !p) -> (l, p))
                  . getDifferential
                      (Status "A")
                      (Status "B")
                      (obsToVals vec as)
                  $   (obsToVals vec bs)
    obsToVals features = fmap (flip S.lookupDenseSV features)

-- | Get edgeR differential expression from a two dimensional matrix.
edgeR :: Int -> TwoDMat -> R s [(Name, Double, PValue, FDR)]
edgeR topN mat = do
    let ss     = fmap (T.unpack . unStatus) . _colStatus $ mat
        topN32 = fromIntegral topN :: Int32

    rMat <- fmap unRMat $ twoDMatToRMat mat

    resR <- [r| library(edgeR)

                group = factor(ss_hs)
                y = DGEList(counts = rMat_hs, group = group)
                # Keep genes with at least 1 count per million (cpm) in at least two samples.
                countsPerMillion = cpm(y)
                countCheck = countsPerMillion > 1
                keep = which(rowSums(countCheck) >= 2)
                y = y[keep,]
                # Normalize
                y = calcNormFactors(y)
                design = model.matrix(~ group)
                y = estimateDisp(y, design)
                fit = glmFit(y, design)
                lrt = glmLRT(fit, coef = 2)
                res = topTags(lrt, n = topN32_hs)$table

                # return(jsonlite::toJSON(res))
                return(res)
           |]

    -- let df = R.fromSomeSEXP resR :: String
    --     genes = fmap Name $ df L.^.. L.values . L.key "_row" . L._String
    --     vals  = fmap Scientific.toRealFloat
    --           $ df L.^.. L.values . L.key "logFC" . L._Number
    --     pVals = fmap (PValue . Scientific.toRealFloat)
    --           $ df L.^.. L.values . L.key "PValue" . L._Number
    --     fdrs  = fmap (FDR . Scientific.toRealFloat)
    --           $ df L.^.. L.values . L.key "FDR" . L._Number

    genesR <- [r| row.names(resR_hs) |]
    valsR  <- [r| resR_hs$logFC |]
    pValsR <- [r| resR_hs$PValue |]
    fdrsR  <- [r| resR_hs$FDR |]

    let genes = fmap (Name . T.pack) (R.dynSEXP genesR :: [String])
        vals  = R.dynSEXP valsR :: [Double]
        pVals = fmap PValue (R.dynSEXP pValsR :: [Double])
        fdrs  = fmap FDR (R.dynSEXP fdrsR :: [Double])

    return . zip4 genes vals pVals $ fdrs
