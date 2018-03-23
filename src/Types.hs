{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}

module Types where

-- Standard
import Foreign.R.Internal as R
import Language.R.Instance as R
import Language.R.Literal as R
import Language.R.QQ
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.Text as T

-- Local


-- Basic
newtype NameCol = NameCol
    { unNameCol :: T.Text
    } deriving (Eq,Ord,Read,Show)
newtype StatusCol  = StatusCol T.Text
newtype ValueCol   = ValueCol T.Text
newtype PValue = PValue
    { unPValue :: Double
    } deriving (Eq,Ord,Read,Show)
newtype FDR = FDR
    { unFDR :: Double
    } deriving (Eq,Ord,Read,Show)
newtype Name       = Name { unName :: T.Text } deriving (Eq, Ord, Read, Show)
newtype Status = Status
    { unStatus :: T.Text
    } deriving (Eq,Ord,Read,Show)
newtype Comparison = Comparison { unComparison :: T.Text } deriving (Eq, Ord)
newtype NameMap    = NameMap (Map.Map Name (Map.Map Status (Seq.Seq Double)))
newtype RMat s     = RMat { unRMat :: R.SomeSEXP s }

-- Advanced
data Entity = Entity { _name   :: Name
                     , _status :: Status
                     , _value  :: Double
                     }

data TwoDMat = TwoDMat { _rowNames  :: [Name]
                       , _colNames  :: [Name]
                       , _colStatus :: [Status]
                       , _numRows   :: Int
                       , _numCols   :: Int
                       , _matrix    :: [(Int, Int, Double)]
                       }


newtype ComparisonMap = ComparisonMap { unComparisonMap :: Map.Map Comparison PValue }
newtype OutputMap = OutputMap { unOutputMap :: Map.Map T.Text Double }
