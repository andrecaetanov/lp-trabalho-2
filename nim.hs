-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Data.Char

start :: IO ()
start = do
    putStrLn "The game started!"
    play initialRows 2

initialRows :: [Int]
initialRows = [1, 3, 5, 7]

play :: [Int] -> Int -> IO ()
play rows player = do
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
            play (removeSticks rows (fst move) (snd move)) (nextPlayer player)
        else do
            putStrLn "Opponent's turn!"
            putStrLn "The opponent made a move."
            play (removeSticks rows (fst (bestPlay rows)) (snd (bestPlay rows))) (nextPlayer player)

printRows :: [Int] -> IO ()
printRows rows = do
    putStrLn "Number of sticks per row:"
    mapM_ print rows
    putChar '\n'

removeSticks :: [Int] -> Int -> Int -> [Int]
removeSticks rows chosenRow sticksRemoved = 
    [if row == chosenRow then sticks - sticksRemoved else sticks | (sticks, row) <- zip rows [1..length rows]]

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

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

getDigit :: IO Int
getDigit = do
    line <- getLine
    if isDigit (head line) then do 
        return (digitToInt (head line))
    else do 
        putStrLn "Invalid input: The input must be a digit."
        putStr "Type again: "
        getDigit

isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove rows chosenRow sticksRemoved = if (rows !! (chosenRow - 1)) >= sticksRemoved then True else False

bestPlay :: [Int] -> (Int, Int)
bestPlay (x:xs) = bestPlayHelper (x:xs) 1 x

bestPlayHelper :: [Int] -> Int -> Int -> (Int, Int)
bestPlayHelper rows row sticksRemoved = 
    if (and [row == (length rows), sticksRemoved == 0]) then 
        (getFirstNotZero rows, 1)
    else if (sticksRemoved == 0) then
        bestPlayHelper rows (row + 1) (rows !! row)
    else if (checkDigitsEven (sumDigitsBin (replace rows row ((rows !! (row - 1)) - sticksRemoved)))) then
        (row, sticksRemoved)
    else
        bestPlayHelper rows row (sticksRemoved - 1)

getFirstNotZero :: [Int] -> Int
getFirstNotZero [] = 0
getFirstNotZero (x:xs) = if (x /= 0) then x else getFirstNotZero xs

replace :: [Int] -> Int -> Int -> [Int]
replace [] _ _ = []
replace (x:xs) i n = if (i == 1) then n : xs else x : (replace xs (i - 1) n)

sumDigitsBin :: [Int] -> [Int]
sumDigitsBin [] = []
sumDigitsBin (x:xs) = sumDigits (toBin3 x) (sumDigitsBin xs)

sumDigits :: [Int] -> [Int] -> [Int]
sumDigits [] [] = []
sumDigits (x:xs) [] = (x:xs)
sumDigits [] (x:xs) = (x:xs)
sumDigits (x:xs) (y:ys) = x + y : sumDigits xs ys

toBin3 :: Int -> [Int]
toBin3 n = concat [take (3 - (length (toBin n))) (repeat 0), (toBin n)]

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (toBinHelper n)

toBinHelper :: Int -> [Int]
toBinHelper 0 = []
toBinHelper n = let (q,r) = n `divMod` 2 in r : toBinHelper q

checkDigitsEven :: [Int] -> Bool
checkDigitsEven [] = True
checkDigitsEven (x:xs) = if ((x `mod` 2) /= 0) then False else checkDigitsEven xs