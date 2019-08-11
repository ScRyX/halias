{-# LANGUAGE OverloadedStrings #-}

module CSS (designCss) where

import Clay
import Data.Text.Lazy
import Clay.Color

designCss :: Text
designCss =
  render $ do
    body ? minHeight (px 2000)
    element ".blueCard" ? do
      color white
      background blue
    element ".redCard" ? do
      color white
      background red
    element ".beigeCard" ? do
      color black
      background beige
    element ".blackCard" ? do
      color white
      background black
    element ".card" ? do
      padding (px 10) (px 10) (px 10) (px 10)
      margin  (px 10) (px 10) (px 10) (px 10)
      textAlign Clay.center

