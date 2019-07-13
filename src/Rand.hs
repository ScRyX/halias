module Rand where

import System.Random

randInt :: IO Int
randInt = fst . next <$> getStdGen

randoms :: StdGen -> [Int]
randoms = randomRs (0, 1000)

