{-# LANGUAGE  OverloadedStrings #-}

module Api where

import Data.IORef
import Web.Scotty
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as L

counter :: IORef Int -> ScottyM ()
counter ref = get "/count" (showCounterAndIncrease ref)

showCounterAndIncrease :: IORef Int -> ActionM ()
showCounterAndIncrease ref = do
  currentVal <- liftIO $ readIORef ref
  liftIO $ modifyIORef ref (+ 1)
  text (L.pack $ show currentVal)

runCounter :: IO ()
runCounter = do
  ref <- newIORef (0 :: Int)
  scotty 3000 (counter ref)

