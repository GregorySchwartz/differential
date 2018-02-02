{- differential
Gregory W. Schwartz

Finds out whether an entity comes from different distributions (statuses).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Standard
import Data.List
import Data.Maybe
import Data.Semigroup
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Cabal
import qualified Data.Csv as CSV
import Options.Generic
import TextShow
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Vector as V
       
import qualified H.Prelude as H
import qualified Foreign.R as R
import Language.R.Instance as R
-- import Language.R.Literal as R

-- Local
import Types
import Load
import Differential

-- | Command line arguments
data Options = Options { nameCol  :: Maybe T.Text
                               <?> "([name] | COLUMN) The column containing the names of the entities."
                       , statusCol :: Maybe T.Text
                               <?> "([status] | COLUMN) The column containing the statuses of the entities."
                       , valueCol :: Maybe T.Text
                               <?> "([value] | COLUMN) The column containing the values of the entities."
                       }
               deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "differential, Gregory W. Schwartz.\
                      \ Finds out whether an entity comes from\
                      \ different distributions (statuses)."

    contents <- BL.getContents

    let nameCol'   = NameCol . fromMaybe "name" . unHelpful . nameCol $ opts
        statusCol' =
            StatusCol . fromMaybe "status" . unHelpful . statusCol $ opts
        valueCol'  = ValueCol . fromMaybe "value" . unHelpful . valueCol $ opts
        rows = either error snd
             $ (CSV.decodeByName contents :: Either String (CSV.Header, V.Vector (Map.Map T.Text T.Text)))
        entities = fmap (toEntity nameCol' statusCol' valueCol') rows
        nameMap  = getNameMap entities

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        outputMaps <- getDifferentials nameMap

        let comparisons = Set.toList
                        . Set.fromList
                        . concatMap (Map.keys . unOutputMap . snd)
                        $ outputMaps
            outputHeader = V.fromList
                         $ [B.pack . T.unpack . unNameCol $ nameCol']
                        <> fmap (B.pack . T.unpack) comparisons
            outputBody   =
                fmap ( (\ m -> foldl'
                                (\acc x -> Map.insertWith (flip const) x "" acc)
                                m
                                comparisons
                       )
                     . (\ (!n, !m) -> Map.insert (unNameCol nameCol') (unName n)
                                  . Map.map showt
                                  . unOutputMap
                                  $ m
                       )
                     )
                         $ outputMaps
        H.io . BL.putStrLn . CSV.encodeByName outputHeader $ outputBody

    return ()
