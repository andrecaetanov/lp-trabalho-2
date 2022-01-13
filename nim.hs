-- André Caetano Vidal 201665010AC
-- Bernardo Souza Abreu Cruz 201635019

import Player
import Util
import Opponent.Easy
import Opponent.Hard

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
        