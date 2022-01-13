module Player where

-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Util

-- Gets the player's movement as a tuple 
-- Where the first element is the index of the row and the second the number of sticks to be removed
playerMove :: [Int] -> IO (Int, Int)
playerMove rows = do
    putStr "Enter the row number: "
    chosenRow <- getDigit
    if (isValidRow rows chosenRow) then do
        putStr "Enter the number of sticks to be removed: "
        sticksRemoved <- getDigit
        if (isValidMove rows chosenRow sticksRemoved) then do
            return (chosenRow, sticksRemoved)
        else do
            putStrLn "Invalid move: The line doesn't have that many sticks. Try again!"
            playerMove rows
    else do
        putStrLn "Invalid move: Invalid row number. Try again!"
        playerMove rows

-- Checks if the line number is greater than 0 and less or equal to the number of lines
isValidRow :: [Int] -> Int -> Bool
isValidRow rows chosenRow = (chosenRow > 0) && (chosenRow <= (length rows))

-- Checks if the row has a number greater than or equal to the number of sticks to be removed
isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove rows chosenRow sticksRemoved = (rows !! (chosenRow - 1)) >= sticksRemoved