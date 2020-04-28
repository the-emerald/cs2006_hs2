module GameOptions where

import Data.Char
import Text.Read

import Board

---------------------- Functions For Game Settings ----------------------

settingsHandler :: String -> GameState -> Either String GameState
settingsHandler option st = case option of
                              "toggle-ai" -> Right (changeAiPlayer st)
                              "toggle-asp" -> Right (changeASP st)
                              ('s':'i':'z':'e':':':size) -> changeBoardSize size st
                              _ -> Left "[ERROR] Invalid Settings Choice"


-- Changes the current AI player
changeAiPlayer :: GameState -> GameState
changeAiPlayer st = GameState (initBoard (size (board st)) (asp (board st))) undefined False (other (ai st)) (ai st)

changeASP :: GameState -> GameState 
changeASP st = GameState (otherAsp (board st)) undefined False (ai st) (turn st)

changeBoardSize :: String -> GameState -> Either String GameState
changeBoardSize size st = case getSize size of 
                              Nothing -> Left "[ERROR] Invalid Size Entered"
                              Just x -> Right (GameState (initBoard x (asp (board st))) undefined False (ai st) (turn st))

---------------------- Functions For In Game Options ----------------------

-- Used to handle in game options.
optionHandler :: String -> GameState -> Either String GameState
optionHandler option st = case option of 
                            "pass" -> Right (playerPass st)
                            "undo" -> undo st
                            _ -> Left "[ERROR] Invalid Input"


-- If player passes then new state returned with number of passes increased
playerPass :: GameState -> GameState
playerPass st = do let board' = Board (size (board st)) (asp (board st)) ((passes (board st)) + 1) (pieces (board st))
                   GameState board' (previous st) True (ai st) (other (turn st))


-- Undo: Revert to previous move 
undo :: GameState -> Either String GameState 
undo st 
        | canUndo st = Right (previous st)
        | otherwise = Left "[ERROR] Cannot Undo Further"


---------------------- Functions For Validating Command Line Arguments ------------------------

-- Inititates the game state with the user entered arguments
initGameState :: [String] -> Either String GameState
initGameState args 
            | length args == 0 = Left "[INFO] No command line arguments were detected"
            | length args /= 3 = Left ("[ERROR] An invalid number of arguments were entered (" ++ show(length args) ++ ")")
            | otherwise = do let size = getSize (args!!0)
                             let ai = getColour (args!!1)
                             let asp = getASP (args!!2)
                             if size == Nothing || ai == Nothing || asp == Nothing then 
                                 Left "[ERROR] Could not initialise board with given arguments"
                              else 
                                 do let getVal = (\(Just x) -> x)
                                    let size' = getVal size
                                    let ai' = getVal ai
                                    let asp' = getVal asp
                                    Right (GameState (initBoard size' asp') undefined False ai' (other ai'))

-- Parses the entered board size
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


getASP :: String -> Maybe Bool
getASP asp = case asp of 
               "true" -> Just True 
               "false" -> Just False 
               _ -> Nothing