module Main where

import           Api
import           Data.IORef
import           Game
import           Rand
import           View

main :: IO ()
main = runApi

test :: IO ()
test = do
  s <- initState defaultProps
  let h = stateView s "Test View"
  writeFileHtml h "/Users/juraj.citorik/Desktop/halias.html"
