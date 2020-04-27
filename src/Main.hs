module Main where

import System.Environment

import GameOptions
import Board
import Display
import Input
import AI

gameLoop :: GameState -> IO ()
gameLoop st
    | gameOver (board st) = putStrLn "[INFO] Game Over"
    | otherwise = do putStr "Move: "
                     input <- getLine
                     let input' = lowerStr input
                     case nextState input' st of 
                            Left msg -> do putStrLn (showGameState st) 
                                           putStrLn msg
                                           gameLoop st
                            Right st' -> do putStrLn (showGameState st') 
                                            gameLoop st'
          

main :: IO ()
main = do putStrLn "---------- Welcome to Othello ----------"    
          args <- getArgs
          let args' = map lowerStr args
          case initGameState args' of 
              Left msg -> do putStrLn msg
                             putStrLn "[INFO] Default board used\n"
                             putStrLn (showGameState defaultGameState)
                             gameLoop (defaultGameState)
              Right st -> do putStrLn "[INFO] Board created with command line arguments\n"
                             putStrLn (showGameState st) 
                             gameLoop (st)
