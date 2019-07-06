{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import           TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"
