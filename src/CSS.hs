{-# LANGUAGE OverloadedStrings #-}

module CSS
  ( designCss
  ) where

import           Clay
import           Clay.Color
import           Data.Text.Lazy

designCss :: Text
designCss =
  render $ do
    body ? minHeight (px 2000)
    element ".grayCard" ? do
      color white
      background gray
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
      margin (px 10) (px 10) (px 10) (px 10)
      textAlign Clay.center
