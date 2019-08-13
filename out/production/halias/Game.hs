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

data Team = BlueTeam | RedTeam deriving (Eq, Show)
data CardType = Blue | Red | Assassin | Neutral deriving (Eq, Show)

data GameProps = GameProps {_nBlue :: Int, _nRed :: Int, _nAssassin :: Int, _nTotal :: Int}
makeLenses ''GameProps
nOther :: Lens' GameProps Int
nOther f p = fmap (const p) (f $ p ^. nTotal - (p ^. nBlue + p ^. nRed + p ^. nAssassin))

data CardState = Picked | Unpicked deriving (Eq, Show)
data Card = Card {word :: T.Text, typ :: CardType, state :: CardState} deriving (Eq, Show)
data GameState = GameState {_cards :: [Card], _turn :: Team, _winner :: Maybe Team} deriving (Eq, Show)
makeLenses ''GameState

-- TODO check for duplicates in the wordSet and fail if there's < nTotal distinct words

mkCard :: CardType -> T.Text -> Card
mkCard t w = Card {word = w, typ = t, state = Unpicked}

defaultProps :: GameProps
defaultProps = GameProps { _nBlue = 7, _nRed = 8, _nAssassin = 1, _nTotal = 25}

initialTurn :: GameProps -> Team
initialTurn props
  | props ^. nBlue > props ^. nRed = RedTeam
  | otherwise = BlueTeam

chooseWords :: GameProps -> [T.Text] -> IO [T.Text]
chooseWords props = chooseN (props^.nTotal)

readWords :: IO [T.Text]
readWords = T.lines <$> TIO.readFile "resources/words.txt"

initState :: GameProps -> IO GameState
initState props = do
  allWords <- readWords
  gameWords <- chooseWords props allWords
  let [blue, red, black, other] =
        splitPlaces [props ^. nBlue, props ^. nRed, props ^. nAssassin, props ^. nOther] gameWords
  cards <-
    randSort $ concat [mkCard Blue <$> blue, mkCard Red <$> red, mkCard Assassin <$> black, mkCard Neutral <$> other]
  return $ GameState {_cards = cards, _turn = initialTurn props, _winner = Nothing}



