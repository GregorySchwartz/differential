{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

module Types where

-- Standard
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.Text as T

-- Local


-- Basic
newtype NameCol    = NameCol { unNameCol :: T.Text }
newtype StatusCol  = StatusCol T.Text
newtype ValueCol   = ValueCol T.Text
newtype PValue     = PValue { unPValue :: Double } deriving (Eq, Ord)
newtype Name       = Name { unName :: T.Text } deriving (Eq, Ord)
newtype Status     = Status T.Text deriving (Eq, Ord)
newtype Comparison = Comparison { unComparison :: T.Text } deriving (Eq, Ord)

-- Advanced
newtype NameMap = NameMap (Map.Map Name (Map.Map Status (Seq.Seq Double)))

data Entity = Entity { _name   :: !Name
                     , _status :: !Status
                     , _value  :: !Double
                     }

newtype ComparisonMap = ComparisonMap { unComparisonMap :: Map.Map Comparison PValue }
newtype OutputMap = OutputMap { unOutputMap :: Map.Map T.Text Double }
