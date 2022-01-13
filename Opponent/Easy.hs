module Opponent.Easy where

-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import System.Random

-- Get a random move as a tuple
-- Where the first element is the index of the row and the second the number of sticks to be removed
randomMove :: [Int] -> IO (Int, Int)
randomMove rows = do
    randomRow <- (getRandomRow rows)
    randomSticks <- randomRIO(1, snd randomRow)
    return (fst randomRow, randomSticks)

-- Get a random row as a tuple
-- Where the first element is the index of the row and the second the number of sticks to be removed
getRandomRow :: [Int] -> IO (Int, Int)
getRandomRow rows = do
    randomIndex <- randomRIO(0, ((length (getNonZeroRows rows)) - 1))
    return ((getNonZeroRows rows) !! randomIndex)

-- Gets a list of non-zero rows 
-- Where each element is a tuple containing the index and value of the row
getNonZeroRows :: [Int] -> [(Int, Int)]
getNonZeroRows rows = getNonZeroRowsHelper rows 1

-- Helper function to get a list of non-zero rows
getNonZeroRowsHelper :: [Int] -> Int -> [(Int, Int)]
getNonZeroRowsHelper [] _ = []
getNonZeroRowsHelper (0:xs) i = getNonZeroRowsHelper xs (i + 1)
getNonZeroRowsHelper (x:xs) i = (i, x) : getNonZeroRowsHelper xs (i + 1)