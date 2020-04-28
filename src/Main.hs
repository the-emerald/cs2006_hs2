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
                     case input' of 
                         "settings" -> settingsLoop st
                         input -> case nextState input st of       
                                        Left msg -> do putStrLn (showGameState st) 
                                                       putStrLn msg
                                                       gameLoop st
                                        Right st' -> do putStrLn (showGameState st') 
                                                        gameLoop st'
          

settingsLoop ::GameState -> IO ()
settingsLoop st = do putStrLn "-------- Settings --------"
                     putStrLn "INSTRUCTIONS: Enter command enclosed in [] to modify setting. Current Setting enclosed in ()."
                     putStrLn ("1. Change AI Player (" ++ show(ai st) ++ ") [toggle-ai] (Game Will Restart)")
                     putStrLn ("2. Change Board Size (" ++ show(size (board st)) ++ ") [size:<new size>] (Game Will Restart)")
                     putStrLn ("3. Allow for alternative starting positions (" ++ show(asp (board st)) ++ ") [toggle-asp] (Game Will Restart)")
                     putStrLn "[exit] ettings menu"
                     putStr "choice: "

                     input <- getLine
                     case input of 
                         "exit" -> do putStrLn "[INFO] Settings menu exited"
                                      putStrLn (showGameState st)
                                      gameLoop st
                         input -> case settingsHandler input st of
                                            Left msg -> do putStrLn msg
                                                           settingsLoop st
                                            Right st' -> do putStrLn "[DONE] Setting updated"
                                                            settingsLoop st'
                                        



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
