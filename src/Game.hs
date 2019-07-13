{-# LANGUAGE OverloadedStrings #-}

module Game
    ( someFunc
    ) where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

words :: [T.Text]
words = ["one", "two", "three", "four"]
