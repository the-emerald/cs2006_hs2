module Main where

import System.Environment

import GameOptions
import Board
import Display
import Input
import AI

gameLoop :: GameState -> IO ()
gameLoop st
    | gameOver (board st) = putStrLn "[INFO] Game Over"                                     -- If the game over detected then end the game
    | otherwise = do putStr ("[" ++ show(turn st) ++ "] Move: ")                            -- Otherwise ask the user to enter a move (can also be a game option such as 'settings' or 'pass')
                     input <- getLine
                     let input' = lowerStr input                                            -- Convert input to lower case
                     case input' of                                                     
                         "quit" -> putStrLn "[INFO] Game was finished by user"              -- End the game immediately if the user enters 'quit'
                         "settings" -> settingsLoop st                                      -- Run the in-game settings menu if the user enters 'settings'
                         input -> case nextState input st of                                -- Otherwise try and parse the input 
                                        Left msg -> do putStrLn (showGameState st)          -- If the input can not be processed, output the board, 
                                                       putStrLn msg                         -- error message 
                                                       gameLoop st                          -- and ask for another move
                                        Right st' -> do putStrLn (showGameState st')        -- If the input was valid then show the game state created from the input 
                                                        gameLoop st'                        -- ask for another move
          

settingsLoop ::GameState -> IO ()
settingsLoop st = do putStrLn "-------- Settings --------"
                     putStrLn "INSTRUCTIONS: Enter command enclosed in [] to modify setting. Current Setting enclosed in ()."                       -- Print settings instructions with current settings values
                     putStrLn ("1. Change AI Player (" ++ show(ai st) ++ ") [toggle-ai] (Game Will Restart)")
                     putStrLn ("2. Change Board Size (" ++ show(size (board st)) ++ ") [size:<new size>] (Game Will Restart)")
                     putStrLn ("3. Allow for alternative starting positions (" ++ show(asp (board st)) ++ ") [toggle-asp] (Game Will Restart)")
                     putStrLn "[exit] settings menu"
                     putStr "choice: "

                     input <- getLine                                                               -- Get input from the user
                     case input of                                                                  
                         "exit" -> do putStrLn "[INFO] Settings menu exited"                        -- If the user enters 'exit' then print informative message, 
                                      putStrLn (showGameState st)                                   -- Print new game created with updated settings 
                                      gameLoop st                                                   -- ask for next move 
                         input -> case settingsHandler input st of                                  -- Otherwise keep displaying settings menu with settings value to allow user to make multiple changes
                                            Left msg -> do putStrLn msg                             -- If the user enters an invalid option then loop
                                                           settingsLoop st
                                            Right st' -> do putStrLn "[DONE] Setting updated"       -- If the user enters a correct option, then inform the user and loop to allow another setting to be
                                                            settingsLoop st'                        -- modified if need be
                                        


main :: IO ()
main = do putStrLn "---------- Welcome to Othello ----------"       
          args <- getArgs                                                                       -- Get the command line arguments
          let args' = map lowerStr args                                                         -- Convert the arguments to lower case strings. Using map to apply function to each string in list of strings
          case initGameState args' of                                                           -- Try to initialise game from the arguments given 
              Left msg -> do putStrLn msg                                                       -- If game state couldnt be created from arguments, return meaningful message 
                             putStrLn "[INFO] Default board used"                               -- Run game with default board and inform the user that this is the case
                             putStrLn (showGameState defaultGameState)
                             gameLoop (defaultGameState)
              Right st -> do putStrLn "[INFO] Board created with command line arguments"        -- If game could be created with command line arguments then inform the user 
                             putStrLn (showGameState st)                                        -- show board 
                             gameLoop (st)                                                      -- start game loop and ask for next move
