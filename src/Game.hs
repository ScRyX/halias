{-# LANGUAGE OverloadedStrings #-}

module Game where
--    ( initState
--    , defaultProps
--    ) where

import qualified Data.Text as T (Text, lines)
import Data.Text.IO as TIO (readFile)
import Rand (chooseN)
import Data.List.Split (splitPlaces)

data Team = BlueTeam | RedTeam
data CardType = Blue | Red | Assassin | Neutral deriving (Eq, Show)

data GameProps = GameProps {nBlue :: Int, nRed :: Int, nAssassin :: Int, nTotal :: Int}

data CardState = Picked | Unpicked deriving (Eq, Show)
data Card = Card {word :: T.Text, typ :: CardType, state :: CardState} deriving (Eq, Show)
type GameState = [Card]

-- TODO check for duplicates in the wordSet and fail if there's < nTotal distinct words

defaultProps :: GameProps
defaultProps = GameProps { nBlue = 7, nRed = 8, nAssassin = 1, nTotal = 25}

chooseWords :: GameProps -> [T.Text] -> IO [T.Text]
chooseWords props = chooseN (nTotal props)

readWords :: IO [T.Text]
readWords = T.lines <$> TIO.readFile "resources/words.txt"


initState :: GameProps -> IO GameState
initState props = do
  allWords <- readWords
  gameWords <- chooseWords props allWords
  let [blue, red, black] = splitPlaces [nBlue props, nRed props, nAssassin props] gameWords
  return $ concat
    [(\w -> Card {word = w, typ = Blue, state = Unpicked}) <$> blue
    ,(\w -> Card {word = w, typ = Red, state = Unpicked}) <$> red
    ,(\w -> Card {word = w, typ = Assassin, state = Unpicked}) <$> black]



