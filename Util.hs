module Util where

-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Data.Char

-- Get a digit as input
getDigit :: IO Int
getDigit = do
    line <- getLine
    if isDigit (head line) then do 
        return (digitToInt (head line))
    else do 
        putStrLn "Invalid input: The input must be a digit."
        putStr "Type again: "
        getDigit

-- Prints the number of sticks in each row
printRows :: [Int] -> IO ()
printRows rows = do
    putStrLn "Number of sticks per row:"
    mapM_ print rows
    putChar '\n'

-- Removes sticks from a given row
removeSticks :: [Int] -> Int -> Int -> [Int]
removeSticks rows chosenRow sticksRemoved = 
    [if row == chosenRow then sticks - sticksRemoved else sticks | (sticks, row) <- zip rows [1..length rows]]

-- Determines the next player from the current player
nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1