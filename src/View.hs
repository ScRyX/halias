{-# LANGUAGE OverloadedStrings #-}
module View where

import Game
import Control.Monad (forM_)
import Data.ByteString.Lazy as L
import Data.List.Split (divvy)
import Text.Blaze.Html5 as H
import Text.Blaze.Renderer.Utf8
import Data.Text as T (concat, pack)
--import qualified Text.Blaze.Html5.Attributes as A

cardView :: Card -> H.Html
cardView c = H.td (toHtml $ T.concat [word c, "[", T.pack $ show (typ c), "]"])

cardRow :: [Card] -> H.Html
cardRow cs = H.tr $ forM_ cs cardView

stateView :: GameState -> H.Html
stateView s =
  H.docTypeHtml $ do
    H.head $ H.title "h-Alias v.0.0.1"
    H.body $ do
      H.p "Here are the cards:"
      H.table $ forM_ (divvy 5 5 s) cardRow

showStateHtml :: H.Html -> L.ByteString
showStateHtml = renderHtml

writeFileHtml :: H.Html -> FilePath -> IO ()
writeFileHtml h p = L.writeFile p $ showStateHtml h