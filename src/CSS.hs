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
      color white
      background beige
    element ".blackCard" ? do
      color white
      background black
--    element "#gmail" ? marginLeft (px 10)

