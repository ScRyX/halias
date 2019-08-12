module Main where

import Game
import Rand
import Data.IORef
import Api
import Web.Scotty

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  scotty 3000 (counter ref)
