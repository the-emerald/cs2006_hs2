module Main where

import Board
import Display
import Input
import AI

gameLoop :: GameState -> IO ()
gameLoop st
    | gameOver (board st) = putStrLn "[INFO] Game Over"
    | otherwise = do putStr "Move: "
                     input <- getLine
                     case nextState input st of 
                         Left msg -> do putStrLn msg
                                        putStrLn (showGameState st) 
                                        gameLoop st
                         Right st' -> do putStrLn (showGameState st') 
                                         gameLoop st'
          

main :: IO ()
main = do let st = initGameState 8
          putStrLn (showGameState st) 
          gameLoop (st)
