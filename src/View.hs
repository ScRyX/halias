{-# LANGUAGE OverloadedStrings #-}

module View where

import           Control.Lens                  ((^.))
import           Control.Monad                 (forM_)
import           CSS                           (designCss)
import           Data.ByteString.Lazy          as L
import           Data.List.Split               (divvy)
import           Data.Text                     as T (Text, concat, pack)
import           Data.Text.Lazy                as LT (toStrict)
import           Game
import           Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Internal           (preEscapedText)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

cardClass :: Card -> AttributeValue
cardClass c =
  case state c of
    Picked ->
      case typ c of
        Blue     -> "blueCard card col-md-2"
        Red      -> "redCard card col-md-2"
        Neutral  -> "grayCard card col-md-2"
        Assassin -> "blackCard card col-md-2"
    Unpicked -> "beigeCard card col-md-2"

cardView :: Card -> H.Html
cardView c = H.div ! A.class_ (cardClass c) $ toHtml (T.concat [word c, " [", T.pack $ show (typ c), "]"])

cardRow :: [Card] -> H.Html
cardRow cs = H.div ! A.class_ "row" $ forM_ cs cardView

pet = preEscapedText

stateView :: GameState -> String -> H.Html
stateView s label =
  H.docTypeHtml $ do
    H.head $ do
      H.title "h-Alias v.0.0.1"
      link ! A.href "https://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! A.rel "stylesheet" !
        A.media "screen"
      style $ pet $ LT.toStrict designCss
    H.body $ do
      H.h1 $ H.strong $ toHtml label
      H.div ! A.class_ "container" $ do
        H.table $ forM_ (divvy 5 5 $ s ^. cards) cardRow
        script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
        script ! A.src "https://netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

showStateHtml :: H.Html -> L.ByteString
showStateHtml = renderHtml

writeFileHtml :: H.Html -> FilePath -> IO ()
writeFileHtml h p = L.writeFile p $ showStateHtml h
