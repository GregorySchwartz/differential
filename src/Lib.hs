{- Differential
Gregory W. Schwartz

Collects the functions pertaining to finding the differential between groups for
each entity.
-}

{-# LANGUAGE QuasiQuotes #-}

module Differential
    ( someFunc
    ) where

-- Standard
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local

-- | Find the p-value of two samples.
differential :: VU.Vector Double -> VU.Vector Double -> R s PValue
differential xs ys = [r| wilcox.test(xs_hs, ys_hs)$p.value |]
             
-- | Convert 
getNameMap :: VU.Vector (Map.Map T.Text T.Text)
getNameMap = 

-- | Gather rows into a map of names with all of their entities.
getNameMap :: VU.Vector (Map.Map T.Text T.Text)
getNameMap = 
    
