module Main where

import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans.Class

import GameOptions
import Board
import Display
import Input
import AI

gameLoop :: GameState -> InputT IO()
gameLoop st
  | gameOver (board st) = outputStrLn ("[INFO] Game Over. " ++ getWinner (board st))    -- If the game over detected then end the game
  
  | turn st == ai st = do outputStr ("[" ++ show (turn st) ++ "] Move: AI\n")           -- If its AI turn then play AI move
                          let st' = updateGameState st
                          outputStrLn (showGameState st')
                          gameLoop st'

  | otherwise = do outputStr ("[" ++ show (turn st) ++ "] Move: ")                      -- Otherwise ask the user to enter a move (can also be a game option such as 'settings' or 'pass')
                   input <- getInputLine ""
                   let input' = lowerStr ((\(Just x) -> x) input)                       -- Convert input to lower case
                   case input' of
                     "quit"     -> outputStrLn "[INFO] Game was finished by user"       -- End the game immediately if the user enters 'quit'
                     
                     "settings" -> do outputStrLn (optionsMenu st)
                                      settingsLoop st                                   -- Run the in-game settings menu if the user enters 'settings'
                     
                     input      -> case nextState input st of                           -- Otherwise try and parse the input
                                      Left msg -> do outputStrLn (showGameState st)     -- If the input can not be processed, output the board,
                                                     outputStrLn msg                    -- error message
                                                     gameLoop st                        -- and ask for another move
                                      Right st' -> do outputStrLn (showGameState st')   -- If the input was valid then show the game state created from the input
                                                      gameLoop st'                      -- ask for another move
          

settingsLoop ::GameState -> InputT IO()
settingsLoop st = do outputStr "Option: "
                     input <- getInputLine ""                                                           -- Get input from the user
                     let input' = lowerStr ((\(Just x) -> x) input)                                     -- Convert input to lower case
                     case input' of                                                                  
                         "exit" -> do outputStrLn "[INFO] Settings menu exited"                         -- If the user enters 'exit' then print informative message, 
                                      outputStrLn (showGameState st)                                    -- Print new game created with updated settings 
                                      gameLoop st                                                       -- ask for next move 
                         input -> case settingsHandler input st of                                      -- Otherwise keep displaying settings menu with settings value to allow user to make multiple changes
                                            Left msg -> do outputStrLn msg                              -- If the user enters an invalid option then loop
                                                           settingsLoop st
                                            Right st' -> do outputStr "[DONE] Setting updated\n"        -- If the user enters a correct option, then inform the user and loop to allow another setting to be
                                                            settingsLoop st'                            -- modified if need be
                                        


main :: IO ()
main = runInputT defaultSettings run
  where
    run :: InputT IO ()
    run = do
      outputStrLn "---------- Welcome to Othello ----------"
      args <- lift getArgs                                                -- Get the command line arguments
      let args' = map lowerStr args                                       -- Convert the arguments to lower case strings. Using map to apply function to each string in list of strings
      case initGameState args' of                                         -- Try to initialise game from the arguments given
        Left msg -> do                                                    -- If game state couldnt be created from arguments, return meaningful message
          outputStrLn msg
          outputStrLn "[INFO] Default board used"
          outputStrLn (showGameState defaultGameState)
          gameLoop defaultGameState
        Right st -> do
          outputStrLn "[INFO] Board created with command line arguments"   -- If game could be created with command line arguments then inform the user
          outputStrLn (showGameState st)                                   -- Show board
          gameLoop st                                                      -- Start game loop and ask for next move
