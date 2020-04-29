module GameOptions where

import Data.Char
import Text.Read

import Board

---------------------- Functions For Game Settings ----------------------

-- The settings handler is used to allow the user to alter game settings whilst the program is running.
-- If an option is entered correctly with correct values then a new game state with the updated field will be returned
settingsHandler :: String -> GameState -> Either String GameState
settingsHandler option st = case option of
                              "toggle-ai" -> Right (changeAiPlayer st)                 -- Changes AI player
                              "toggle-asp" -> Right (changeASP st)                     -- Changes Alternative Starting Positions Value
                              ('s':'i':'z':'e':':':size) -> changeBoardSize size st    -- Alters The Board Size
                              _ -> Left "[ERROR] Invalid Settings Choice"              -- Otherwise input is invalid and error returned


-- Changes the current AI player
changeAiPlayer :: GameState -> GameState
changeAiPlayer st = GameState (initBoard (size (board st)) (asp (board st)))  -- Reset board to starting position           
                              undefined                                       -- Board reset so no alternative state exists
                              False                                           -- There is no option for undo 
                              (other (ai st))                                 -- Change the colour of the AI player Black -> White or White -> Black
                              (ai st)                                         -- Change turn to previous AI colour 


-- Changes whether alternative starting positions is enabled or disabled
changeASP :: GameState -> GameState 
changeASP st = GameState (otherAsp (board st))  -- Reset board with alternative starting positions enabled/disabled
                         undefined              -- There is no defined previous state
                         False                  -- Therefore the player can not undo a move 
                         (ai st)                -- Retain the colours of the AI player
                         (turn st)              -- and the colour of the current turn


-- Parses and verifies the entered size and returns new state with size of board modified 
-- If an invalid size is entered an error message is returned 
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
            | length args == 0 = Left "[INFO] No command line arguments were detected"                                        -- If no arguments were entered, return an error message
            | length args /= 3 = Left ("[ERROR] An invalid number of arguments were entered (" ++ show(length args) ++ ")")   -- If invalid number of arguments entered then reurn another meaningful message
            | otherwise = do let size = getSize (args!!0)                                                                     -- At this point it is safe to extract the values from the arguments 
                             let ai = getColour (args!!1)                                                                     -- The arguments are parsed using their respective parsing function
                             let asp = getASP (args!!2)
                             if size == Nothing || ai == Nothing || asp == Nothing then                                       -- If parsing failed for any of the arguments, or invalid input given 
                                 Left "[ERROR] Could not initialise board with given arguments"                               -- Another meningful message is returned
                              else 
                                 do let getVal = (\(Just x) -> x)                                                             -- Function declared to extract value from maybe value
                                    let size' = getVal size                                                                   -- As parsing was proven to be succesfull for all methods
                                    let ai' = getVal ai                                                                       -- Values can be extracted and stored in variables to maintain readability 
                                    let asp' = getVal asp
                                    Right (GameState (initBoard size' asp') undefined False ai' (other ai'))                  -- New state is returned with command line options present 


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


-- Parses true/false from given string
getASP :: String -> Maybe Bool
getASP asp = case asp of 
               "true" -> Just True 
               "false" -> Just False 
               _ -> Nothing