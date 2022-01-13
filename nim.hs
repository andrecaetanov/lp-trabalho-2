-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Data.Char
import System.Random

-- Starts the game and selecting a difficulty
start :: IO ()
start = do
    putStrLn "The game started!"
    putStr "Select the difficulty (enter 0 for easy and 1 for hard): "
    difficulty <- getDigit
    if (difficulty == 0) then
        play initialRows 1 difficulty
    else
        play initialRows 2 difficulty

-- Representation in list of the game rows with the number of sticks
initialRows :: [Int]
initialRows = [1, 3, 5, 7]

-- Runs a game round that can be either the player's or the opponent's on both difficulties
play :: [Int] -> Int -> Int -> IO ()
play rows player difficulty = do
    putChar '\n'
    printRows rows
    if (all (==0) rows) then
        if player == 1 then do
            putStrLn "You lose!"
        else do
            putStrLn "You won!"
    else
        if player == 1 then do
            putStrLn "Your turn!"
            move <- playerMove rows
            play (removeSticks rows (fst move) (snd move)) (nextPlayer player) difficulty
        else do
            putStrLn "Opponent's turn!"
            putStrLn "The opponent made a move."
            if (difficulty == 0) then do
                move <- randomMove rows
                play (removeSticks rows (fst move) (snd move)) (nextPlayer player) difficulty
            else do
                move <- bestPlay rows
                play (removeSticks rows (fst move) (snd move)) (nextPlayer player) difficulty

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

-- Checks if the move is valid
isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove rows chosenRow sticksRemoved = if (rows !! (chosenRow - 1)) >= sticksRemoved then True else False

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
    randomIndex <- randomRIO(1, ((length (getNonZeroRows rows)) - 1))
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
        