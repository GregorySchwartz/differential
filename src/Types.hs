{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

-- Remote
import Control.DeepSeq (NFData (..))
import Foreign.R.Internal as R
import Language.R.Instance as R
import Language.R.Literal as R
import Language.R.QQ
import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
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
    } deriving (Eq,Ord,Read,Show,Generic)
newtype Log2Diff = Log2Diff
    { unLog2Diff :: Double
    } deriving (Eq,Ord,Read,Show,Generic)
newtype FDR = FDR
    { unFDR :: Double
    } deriving (Eq,Ord,Read,Show,Generic)
newtype QValue = QValue
    { unQValue :: Double
    } deriving (Eq,Ord,Read,Show,Generic)
newtype Name       = Name { unName :: T.Text } deriving (Eq, Ord, Read, Show, A.ToJSON,Generic)
newtype Id         = Id { unId :: T.Text } deriving (Eq, Ord, Read, Show, A.ToJSON,Generic)
newtype Status = Status
    { unStatus :: T.Text
    } deriving (Eq,Ord,Read,Show, A.ToJSON)
newtype Comparison = Comparison { unComparison :: T.Text } deriving (Eq, Ord)
newtype NameMap    = NameMap (Map.Map Name (Map.Map Status (Seq.Seq Double)))
newtype RMat s     = RMat { unRMat :: R.SomeSEXP s }

instance NFData Log2Diff
instance NFData PValue
instance NFData QValue
instance NFData FDR
instance NFData Name
instance NFData Id

-- Advanced
data Entity = Entity { _name   :: Name
                     , _status :: Status
                     , _id     :: Id
                     , _value  :: Double
                     }
              deriving (Read, Show, Generic)

instance A.ToJSON Entity where
  toJSON = A.genericToJSON A.defaultOptions{ A.fieldLabelModifier = drop 1 }

data TwoDMat = TwoDMat { _rowNames  :: [Name]
                       , _colNames  :: [Name]
                       , _colStatus :: [Status]
                       , _numRows   :: Int
                       , _numCols   :: Int
                       , _matrix    :: [(Int, Int, Double)]
                       }


newtype ComparisonMap = ComparisonMap { unComparisonMap :: Map.Map Comparison (Maybe PValue) }
newtype OutputMap = OutputMap { unOutputMap :: Map.Map T.Text T.Text }
