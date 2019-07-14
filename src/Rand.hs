module Rand
    ( chooseN
    ) where

import System.Random
import Data.List

randInt :: IO Int
randInt = fst . next <$> getStdGen

randomInts :: StdGen -> [Int]
randomInts = randomRs (0, 1000)

randSublist :: StdGen -> Int -> [a] -> [a]
randSublist g n xs = take n $ snd <$> sortBy (\(a, _) (b, _) -> compare a b) (zip (randomInts g) xs)

chooseN :: Int -> [a] -> IO [a]
chooseN n xs = do
  g <- newStdGen
  return $ randSublist g n xs

