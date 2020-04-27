module GameOptions where

import Data.Char
import Text.Read

import Board

-- Used to handle user entered options.
optionHandler :: String -> GameState -> Either String GameState
optionHandler option st = case option of 
                            "pass" -> Right (playerPass st)
                            "undo" -> undo st
                            _ -> Left "[ERROR] Invalid Input"


-- If player passes then new state returned with number of passes increased
playerPass :: GameState -> GameState
playerPass st = do let board' = Board (size (board st)) ((passes (board st)) + 1) (pieces (board st))
                   GameState board' (previous st) True (ai st) (other (turn st))


-- Undo: Revert to previous move 
undo :: GameState -> Either String GameState 
undo st 
        | canUndo st = Right (previous st)
        | otherwise = Left "[ERROR] Cannot Undo Further"


---------------------- Methods For Validating Command Line Arguments ------------------------

-- Inititates the game state
initGameState :: [String] -> Either String GameState
initGameState args 
            | length args == 0 = Left "[INFO] No command line arguments were detected"
            | length args /= 2 = Left ("[ERROR] An invalid number of arguments were entered (" ++ show(length args) ++ ")")
            | otherwise = do let size = getSize (args!!0)
                             let ai = getColour (args!!1)
                             if size == Nothing || ai == Nothing then 
                                 Left "[ERROR] Could not initialise board with given arguments"
                              else 
                                 do let getVal = (\(Just x) -> x)
                                    let size' = getVal size
                                    let ai' = getVal ai
                                    Right (GameState (initBoard size') undefined False ai' (other ai'))


getSize :: String -> Maybe Int 
getSize size = case readMaybe size :: Maybe Int of
                        Just x -> do if x > 0 && x < 27 then
                                         Just x
                                      else
                                         Nothing
                        Nothing -> Nothing 


-- Converts user entered colour (string) into type Col
getColour :: String -> Maybe Col 
getColour colour = case colour of 
                          "black" -> Just Black
                          "white" -> Just White 
                          _ -> Nothing