{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map.Strict               as M
import qualified Data.Text.Lazy                as L
import           Game                          (GameState, defaultProps,
                                                initState)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           View                          (stateView)
import           Web.Scotty

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
  let g = M.lookup game games
  case g of
    Just foundGame -> html $ renderHtml $ stateView foundGame $ "Found game '" ++ game ++ "'"
    Nothing -> do
      newGame <- liftIO $ initState defaultProps
      liftIO $ modifyIORef ref (M.insert game newGame)
      html $ renderHtml $ stateView newGame $ "Game '" ++ game ++ "' not found, creating new one."
