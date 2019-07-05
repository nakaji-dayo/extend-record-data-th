{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Language.Haskell.TH
import           TH

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

mapM mkOptional [''Person]
deriving instance Show Person_opt

-- [extend|Generated { ...Person } (Show, Eq)|]

data Extra = Extra
  { position :: String
  }

[unionRecord|ExtraPerson = Person Extra|]
deriving instance Show ExtraPerson
main :: IO ()
main = do
  let x = Person_opt Nothing Nothing
  print $ x
  -- q <- runQ [d| data X = X { name :: String } deriving (Show) |]
  -- let y = Generated "test" 88
  -- let z = Generated "test" 89
  -- print y
  -- print $ q
  -- print $ y == z
  let e = ExtraPerson "" 0 ""
  print (e :: ExtraPerson)
