module Opponent.Hard where

-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Util
import Opponent.Easy

-- Get a move following the winning strategy
bestPlay :: [Int] -> IO (Int, Int)
bestPlay (x:xs) = bestPlayHelper (x:xs) 1 x

-- Helper function to get a move following the winning strategy
bestPlayHelper :: [Int] -> Int -> Int -> IO (Int, Int)
bestPlayHelper rows row sticksRemoved = 
    if (and [row == (length rows), sticksRemoved == 0]) then do
        (randomMove rows)
    else if (sticksRemoved == 0) then
        bestPlayHelper rows (row + 1) (rows !! row)
    else if (checkDigitsEven (sumDigitsBin (removeSticks rows row sticksRemoved))) then
        return (row, sticksRemoved)
    else
        bestPlayHelper rows row (sticksRemoved - 1)

-- Gets a list where each position is the sum of the digits of the numbers converted to binary
sumDigitsBin :: [Int] -> [Int]
sumDigitsBin [] = []
sumDigitsBin (x:xs) = sumDigits (toBin3 x) (sumDigitsBin xs)

-- Gets a list where each position is the sum of the elements of the same index of two lists
sumDigits :: [Int] -> [Int] -> [Int]
sumDigits [] [] = []
sumDigits (x:xs) [] = (x:xs)
sumDigits [] (x:xs) = (x:xs)
sumDigits (x:xs) (y:ys) = x + y : sumDigits xs ys

-- Converts a number to binary with at least 3 places
toBin3 :: Int -> [Int]
toBin3 n = concat [take (3 - (length (toBin n))) (repeat 0), (toBin n)]

-- Convert a number to binary
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (toBinHelper n)

-- Helper function to convert a number to binary
toBinHelper :: Int -> [Int]
toBinHelper 0 = []
toBinHelper n = let (q,r) = n `divMod` 2 in r : toBinHelper q

-- Checks if all digits (represented in a list) are even
checkDigitsEven :: [Int] -> Bool
checkDigitsEven [] = True
checkDigitsEven (x:xs) = if ((x `mod` 2) /= 0) then False else checkDigitsEven xs