module Main where

import Game
import Rand

main :: IO ()
main = do
  putStrLn "Generating random int..."
  i <- show <$> randInt
  putStrLn i
