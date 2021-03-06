{- Utility
Gregory W. Schwartz

Collects the miscellaneous functions of the program.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Utility
    ( twoDMatToRMat
    , getFDR
    ) where

-- Standard

-- Cabal

import Data.Function (on)
import Data.List (sortBy, genericLength)
import Data.Maybe (catMaybes)
import Data.Int (Int32)
import Language.R.Instance as R
import Language.R.Literal as R
import Language.R.QQ
import qualified Control.Lens as L
import qualified Data.Text as T

-- Local
import Types

-- | Convert the two dimensional matrix to an R sparse matrix. Assumes 0 indexed
-- to 1 indexed.
twoDMatToRMat :: TwoDMat -> R s (RMat s)
twoDMatToRMat mat = do
    let (is, js, vs) =
            unzip3
                . fmap ( L.over L._2 ((+ 1) . toInt32)
                       . L.over L._1 ((+ 1) . toInt32)
                       )
                . _matrix
                $ mat
        rNames = fmap (T.unpack . unName) . _rowNames $ mat
        cNames = fmap (T.unpack . unName) . _colNames $ mat
        nRows  = toInt32 . _numRows $ mat
        nCols  = toInt32 . _numCols $ mat

    rMat <- [r| library(Matrix)

                mat = sparseMatrix( i = is_hs
                                  , j = js_hs
                                  , x = vs_hs
                                  , dims = c(nRows_hs, nCols_hs)
                                  )
                rownames(mat) = rNames_hs
                colnames(mat) = cNames_hs

                return(mat)
            |]

    return . RMat $ rMat

-- | Convert an Int to and Int32.
toInt32 :: Int -> Int32
toInt32 x = fromIntegral x :: Int32

-- | Convert p-values to FDR using the Benjamini-Hochberg procedure.
getFDR :: Double -> [Maybe PValue] -> [(Maybe FDR, Maybe QValue)]
getFDR alpha xs =
  fmap snd
    . sortBy (compare `on` fst)
    . fmap (\(!r, (!o, !p)) -> (o, (getFDRCritical r p, getQValue r p)))
    . drop 1 -- Get rid of starting value
    . scanl scanFunc (0, (-1, Nothing))
    . sortBy (compare `on` snd)
    . zip ([1..] :: [Int])
    $ xs
  where
    m = genericLength . catMaybes $ xs
    scanFunc (!r, (_, !prev)) x@(_, Nothing) = (r, x)
    scanFunc (!r, (_, !prev)) !x =
      if snd x == prev then (r, x) else (r + 1, x) -- Handle ties
    getFDRCritical _ Nothing     = Nothing
    getFDRCritical rank (Just _) = Just . FDR $ alpha * (rank / m)
    getQValue _ Nothing     = Nothing
    getQValue rank (Just (PValue p)) = Just . QValue . min 1 $ p * (m / rank)
