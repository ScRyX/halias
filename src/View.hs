{-# LANGUAGE OverloadedStrings #-}
module View where

import Game
import Control.Monad (forM_)
import Data.ByteString.Lazy as L
import Data.List.Split (divvy)
import Text.Blaze.Html5 as H
import Text.Blaze.Renderer.Utf8
import Text.Blaze.Internal (preEscapedText)
import Data.Text as T (concat, pack, Text)
import Data.Text.Lazy  as LT (toStrict)
import qualified Text.Blaze.Html5.Attributes as A
import CSS (designCss)

cardClass :: Card -> AttributeValue
cardClass c = case typ c of
  Blue     -> "blueCard card col-md-2"
  Red      -> "redCard card col-md-2"
  Neutral  -> "beigeCard card col-md-2"
  Assassin -> "blackCard card col-md-2"

cardView :: Card -> H.Html
cardView c = H.div ! A.class_ (cardClass c) $ toHtml (T.concat [word c, "[", T.pack $ show (typ c), "]"])

cardRow :: [Card] -> H.Html
cardRow cs = H.div ! A.class_ "row" $ forM_ cs cardView

pet = preEscapedText

stateView :: GameState -> H.Html
stateView s =
  H.docTypeHtml $ do
    H.head $ do
      H.title "h-Alias v.0.0.1"
      link ! A.href "https://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! A.rel "stylesheet" ! A.media "screen"
      style $ pet $ LT.toStrict designCss
    H.body
      H.div ! A.class_ "container" $ do
        H.table $ forM_ (divvy 5 5 s) cardRow
        script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
        script ! A.src "https://netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

showStateHtml :: H.Html -> L.ByteString
showStateHtml = renderHtml

writeFileHtml :: H.Html -> FilePath -> IO ()
writeFileHtml h p = L.writeFile p $ showStateHtml h