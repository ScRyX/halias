{-# LANGUAGE  OverloadedStrings #-}

module Api where

import Data.IORef
import Web.Scotty
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import qualified Data.Text.Lazy as L
import qualified Data.Map.Strict as M
import Game (initState, defaultProps, GameState)
import View (stateView)
import Text.Blaze.Renderer.Text (renderHtml)

runApi :: IO ()
runApi = do
  state <- initState defaultProps
  ref <- newIORef (M.fromList [("game1", state)] :: M.Map String GameState)
  scotty 3000 (api ref)
  
api :: IORef (M.Map String GameState) -> ScottyM ()
api ref = 
  get "/:game" $ do
    game <- param "game"
    getGame ref game

getGame :: IORef (M.Map String GameState) -> String -> ActionM ()
getGame ref game = do
  games <- liftIO $ readIORef ref 
  let g =  M.lookup game games
  case g of
    Just foundGame -> html $ renderHtml $ stateView foundGame 
    Nothing        -> do
      newGame <- liftIO $ initState defaultProps 
      liftIO $ modifyIORef ref (M.insert "game1" newGame)
      html $ renderHtml $ stateView newGame

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

