{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import           TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Person = Person
  { name :: String
  , age  :: Int
  }
  deriving (Show)

mapM mkOptional [''Person]
instance Show Person_opt
