{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

-- import           Data.Generics.Labels
-- import           Data.Generics.Product.Subtype
import           Data.Record.Extend
import           GHC.Generics
import           Language.Haskell.TH
-- import           Lens.Micro

data Animal = Animal
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

data HumanInfo = HumanInfo
  { address :: String
  } deriving (Show, Eq, Generic)

$(extendD "data Human = Animal <> HumanInfo deriving (Show, Generic) ")

data X a = X {f1 :: a Int} deriving (Generic)
data Y a = Y {f2 :: Maybe a} deriving (Generic, Show)
data Z a = Z { x :: X a } deriving(Generic)
$(extendD "data XY = X <> Y <> { e1 :: String, e2 :: String } <> Z deriving (Generic)")

main :: IO ()
main = putStrLn "No tests"
  -- let e = Human
  --       { name = "daishi"
  --       , age = 30
  --       , address = "jp"
  --       }
  -- print (e :: Human)
  -- print (upcast e :: Animal)
  -- let xy = XY {f1 = [123], f2 = Just "test", e1="foo", e2="bar", x = X (0, 9)}
  -- print $ (xy ^. #f1, xy ^. #f2, xy ^. #x . #f1)
