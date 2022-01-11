-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Data.Char

start :: IO ()
start = play initialRows 1

initialRows :: [Int]
initialRows = [1, 3, 5, 7]

play :: [Int] -> Int -> IO ()
play rows player = do
    printRows rows
    if player == 1 then
        if all (==0) rows then do
            putStrLn "You lose!"
        else do
            putStrLn "Your turn!"
            putStrLn "Enter the row number: "
            chosenRow <- getDigit
            putStrLn "Enter the number of sticks to be removed: "
            sticksRemoved <- getDigit
            play (removeSticks rows chosenRow sticksRemoved) (nextPlayer player)
    else
        if all (==0) rows then do
            putStrLn "You won!"
        else do
            putStrLn "Enemy turn!"
            play rows (nextPlayer player)

printRows :: [Int] -> IO ()
printRows rows = mapM_ print rows

getDigit :: IO Int
getDigit = do
    x <- getChar
    if isDigit x then do 
        return (digitToInt x)
    else do 
        putStrLn "Invalid input: The input must be a digit."
        putStrLn "Type again: "
        getDigit

removeSticks :: [Int] -> Int -> Int -> [Int]
removeSticks rows chosenRow sticksRemoved = 
    [if row == chosenRow then sticks - sticksRemoved else sticks | (sticks, row) <- zip rows [1..length rows]]

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1