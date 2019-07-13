{-# LANGUAGE OverloadedStrings #-}

module Game
    ( someFunc
    ) where

import qualified Data.Text as T (Text, lines)
import Data.Text.IO as TIO (readFile)

data Team = BlueTeam | RedTeam
data CardType = Blue | Red | Assassin | Neutral

data GameProps = GameProps {nBlue :: Int, nRed :: Int, nAssassin :: Int, nTotal :: Int}

data CardState = Picked | Unpicked
data Card = Card {word :: T.Text, typ :: CardType, state :: CardState}
newtype GameState = GameState [Card]

-- TODO check for duplicates in the wordSet and fail if there's < nTotal distinct words

someFunc :: IO ()
someFunc = putStrLn "someFunc"

words :: IO [T.Text]
words = T.lines <$> TIO.readFile "words.txt"

chooseWords :: [T.Text] -> GameProps -> [T.Text]
chooseWords = undefined
