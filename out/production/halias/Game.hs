{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where
--    ( initState
--    , defaultProps
--    ) where

import qualified Data.Text as T (Text, lines)
import Data.Text.IO as TIO (readFile)
import Rand (randSort, chooseN)
import Data.List.Split (splitPlaces)
import Control.Lens

data Team = BlueTeam | RedTeam
data CardType = Blue | Red | Assassin | Neutral deriving (Eq, Show)

data GameProps = GameProps {_nBlue :: Int, _nRed :: Int, _nAssassin :: Int, _nTotal :: Int}
makeLenses ''GameProps
nOther :: Lens' GameProps Int
nOther f p = fmap (const p) (f $ p ^. nTotal - (p ^. nBlue + p ^. nRed + p ^. nAssassin))

data CardState = Picked | Unpicked deriving (Eq, Show)
data Card = Card {word :: T.Text, typ :: CardType, state :: CardState} deriving (Eq, Show)
type GameState = [Card]

-- TODO check for duplicates in the wordSet and fail if there's < nTotal distinct words

defaultProps :: GameProps
defaultProps = GameProps { _nBlue = 7, _nRed = 8, _nAssassin = 1, _nTotal = 25}

chooseWords :: GameProps -> [T.Text] -> IO [T.Text]
chooseWords props = chooseN (props^.nTotal)

readWords :: IO [T.Text]
readWords = T.lines <$> TIO.readFile "resources/words.txt"

-- assign a random number to each card, sort, and then assign colors from the first

initState :: GameProps -> IO GameState
initState props = do
  allWords <- readWords
  gameWords <- chooseWords props allWords
  let [blue, red, black, other] = splitPlaces [props^.nBlue, props^.nRed, props^.nAssassin, props^.nOther] gameWords
  randSort $ concat
    [
      (\w -> Card {word = w, typ = Blue, state = Unpicked}) <$> blue
    , (\w -> Card {word = w, typ = Red, state = Unpicked}) <$> red
    , (\w -> Card {word = w, typ = Assassin, state = Unpicked}) <$> black
    , (\w -> Card {word = w, typ = Neutral, state = Unpicked}) <$> other
    ]



