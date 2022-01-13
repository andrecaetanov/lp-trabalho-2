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
    putStr "Enter the number of sticks to be removed: "
    sticksRemoved <- getDigit
    if (isValidMove rows chosenRow sticksRemoved) then do
        return (chosenRow, sticksRemoved)
    else do
        putStrLn "Invalid move: The line doesn't have that many sticks. Try again!"
        playerMove rows

-- Checks if the move is valid
isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove rows chosenRow sticksRemoved = (rows !! (chosenRow - 1)) >= sticksRemoved