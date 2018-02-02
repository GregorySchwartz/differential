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
    ) where

-- Standard
import Control.Monad
import Data.Int (Int32 (..))
import Data.List
import Data.Semigroup
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-- Cabal

import Language.R.Instance as R
import Language.R.Literal as R
import Language.R.QQ

-- Local
import Types
import Utility

-- | Get unique pairings of a list. From
-- http://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- | Find the p-value of two samples.
differential :: [Double] -> [Double] -> R s PValue
differential xs ys = [r| suppressWarnings(wilcox.test(xs_hs, ys_hs))$p.value |]
                 >>= (\x -> return . PValue $ ((R.fromSomeSEXP x) :: Double))

-- | Get a comparison.
getDifferential :: Status -> Status -> [Double] -> [Double] -> R s (Comparison, PValue)
getDifferential (Status !s1) (Status !s2) !l1 !l2 = do
    pVal <- differential l1 l2
    return (Comparison (s1 <> "/" <> s2), pVal)

-- | Get all comparisons of a Name.
getNameDifferentials :: Map.Map Status (Seq.Seq Double) -> R s ComparisonMap
getNameDifferentials m = do
    let comparisons = pairs . Map.keys $ m
        comp (!s1, !s2) = getDifferential
                            s1
                            s2
                            (F.toList . (Map.!) m $ s1)
                            (F.toList . (Map.!) m $ s2)

    fmap (ComparisonMap . Map.fromList) . mapM comp $ comparisons

-- | Convert a ComparisonMap to an OutputMap.
comparisonMapToOutputMap :: ComparisonMap -> OutputMap
comparisonMapToOutputMap =
    OutputMap . Map.map unPValue . Map.mapKeys unComparison . unComparisonMap

-- | Get all p-values in all relevant combinations.
getDifferentials :: NameMap -> R s [(Name, OutputMap)]
getDifferentials (NameMap nameMap) =
    sequence
        . Map.elems
        . Map.mapWithKey (\ !k -> fmap ((k,) . comparisonMapToOutputMap)
                                . getNameDifferentials
                         )
        $ nameMap

-- | Get edgeR differential expression from a two dimensional matrix.
edgeR :: Int -> TwoDMat -> R s String
edgeR topN mat = do
    let ss     = fmap (T.unpack . unStatus) . _colStatus $ mat
        topN32 = fromIntegral topN :: Int32

    rMat <- fmap unRMat $ twoDMatToRMat mat

    res <- [r| library(edgeR)

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
               res = topTags(lrt, n = topN32_hs)

               return(res)
           |]

    return (R.fromSomeSEXP res :: String)
