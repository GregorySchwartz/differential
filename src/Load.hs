{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the input data.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Load
    ( toEntity
    , getNameMap
    ) where

-- Standard
import Data.Semigroup
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- Local
import Types

-- | Convert a row to an Entity.
toEntity :: NameCol -> StatusCol -> ValueCol -> Map.Map T.Text T.Text -> Entity
toEntity (NameCol nameCol) (StatusCol statusCol) (ValueCol valueCol) m =
    Entity { _name   = Name . lookupWithErr nameCol $ m
           , _status = Status . lookupWithErr statusCol $ m
           , _value  = either error fst . T.double . lookupWithErr valueCol $ m
           }
  where
    lookupWithErr k =
        Map.findWithDefault (error $ "Cannot find column: " <> show k) k

-- | Gather Entity's, where each entity sharing a name is from a different
-- Status.
getNameMap :: V.Vector Entity -> NameMap
getNameMap =
    NameMap
        . Map.fromListWith (Map.unionWith (Seq.><))
        . fmap (\ !x -> ( _name x
                        , Map.singleton (_status x) (Seq.singleton . _value $ x)
                        )
               )
        . V.toList
