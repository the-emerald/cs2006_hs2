module GameOptions where

import Data.Char
import Text.Read

import Board
import AI
import Data.Maybe (isNothing)

---------------------- Functions For Game Settings ----------------------

-- In game options menu
optionsMenu :: GameState -> String
optionsMenu st =  ("---------------------------------------- Game Options ----------------------------------------"
                  ++ "\n\nINSTRUCTIONS: Enter command enclosed in [] to modify setting. Current Setting enclosed in ()."                   
                  
                  ++ "\n\nBoard - Changes Will Cause Game To Restart:"
                  ++ ("\n  Change AI Player                          [toggle-ai]        (" ++ show(ai st) ++ ")")
                  ++ ("\n  Change Board Size                         [size:<new size>]  (" ++ show(size (board st)) ++ ")")
                  ++ ("\n  Allow for alternative starting positions  [toggle-asp]       (" ++ show(asp (board st)) ++ ")")
                  
                  ++ "\n\nGame Options:"
                  ++ ("\n  AI Level    [ai:<level (1-3)>]                (" ++ show(aiLevel st) ++ ")")
                  ++ "\n  Save Game   [save:<give a unique game name>]"
                  ++ "\n  Reload Game [reload:<game name>]"
                  
                  ++ "\n\n[exit] settings menu\n")
                  ++ "\n---------------------------------------------------------------------------------------------"


-- The settings handler is used to allow the user to alter game settings whilst the program is running.
-- If an option is entered correctly with correct values then a new game state with the updated field will be returned
settingsHandler :: String -> GameState -> Either String GameState
settingsHandler option st = case option of
                              "toggle-ai" -> Right (changeAiPlayer st)                 -- Changes AI player
                              "toggle-asp" -> Right (changeASP st)                     -- Changes Alternative Starting Positions Value
                              ('s':'i':'z':'e':':':size) -> changeBoardSize size st    -- Alters The Board Size
                              ('s':'a':'v':'e':':':name) -> saveGame st name           -- Allows game to be saved (Not Implemented)
                              ('r':'e':'l':'o':'a':'d':':':name) -> reloadGame name    -- Allows game to be reloaded (Not Implemented)
                              ('a':'i':':':level) -> aiLv st level                     -- Allows AI level to be set
                              _ -> Left "[ERROR] Invalid Settings Choice"              -- Otherwise input is invalid and error returned


-- Changes the current AI player
changeAiPlayer :: GameState -> GameState
changeAiPlayer st = GameState (initBoard (size (board st)) (asp (board st)))  -- Reset board to starting position           
                              undefined                                       -- Board reset so no alternative state exists
                              False                                           -- There is no option for undo 
                              (other (ai st))                                 -- Change the colour of the AI player Black -> White or White -> Black
                              (aiLevel st)                                    -- Retain AI level 
                              (ai st)                                         -- Change turn to previous AI colour 


-- Changes whether alternative starting positions is enabled or disabled
changeASP :: GameState -> GameState 
changeASP st = GameState (otherAsp (board st))  -- Reset board with alternative starting positions enabled/disabled
                         undefined              -- There is no defined previous state
                         False                  -- Therefore the player can not undo a move 
                         (ai st)                -- Retain the colours of the AI player
                         (aiLevel st)           -- Retain AI level 
                         (turn st)              -- and the colour of the current turn


-- Parses and verifies the entered size and returns new state with size of board modified 
-- If an invalid size is entered an error message is returned 
changeBoardSize :: String -> GameState -> Either String GameState
changeBoardSize size st = case getSize size of 
                              Nothing -> Left "[ERROR] Invalid Size Entered. Range 4-26. Even Only"
                              Just x -> Right (GameState (initBoard x (asp (board st))) undefined False (ai st) (aiLevel st) (turn st))


-- Saves the current game at current state with an entered name
saveGame :: GameState -> String -> Either String GameState
saveGame st name = Left "[INFO] This feature has not been implemented - see report"


-- Reloads a given game
reloadGame :: String -> Either String GameState
reloadGame name = Left "[INFO] This feature has not been implemented - see report" 


-- Modifies AI Level 
aiLv :: GameState -> String -> Either String GameState
aiLv st level = case getLevel level of
                  Nothing -> Left ("[ERROR] Invalid AI Level (" ++ level ++ "). Range 1-3 Only")
                  Just lv -> Right (GameState (board st) (previous st) (canUndo st) (ai st) lv (turn st))


---------------------- Functions For In Game Options ----------------------

-- Used to handle in game options.
optionHandler :: String -> GameState -> Either String GameState
optionHandler option st = case option of 
                            "pass" -> Right (playerPass st)
                            "undo" -> undo st
                            "hint" -> Left (getHint st)
                            _ -> Left "[ERROR] Invalid Input"


-- Undo: Revert to previous move 
undo :: GameState -> Either String GameState 
undo st 
        | canUndo st = Right (previous (previous st))
        | otherwise = Left "[ERROR] Cannot Undo Further"


-- Hint: IF the player asks for a hint. Then print the best possible move. 
-- AI level was set to three. It is important for the hint to be meaningful 
-- and appear quickly. 
getHint :: GameState -> String 
getHint st =
  case getValidMoves (board st) (turn st) of
    [] -> "[HINT] There are no playable moves. You must [pass]"
    _ -> "[HINT] The AI would play: " ++ mv
  where
    gt = buildTree generateMove (board st) (turn st)
    aiMove = getBestMove 3 gt
    mv = chr (fst aiMove + 65) : show (snd aiMove + 1)


---------------------- Functions For Validating Command Line Arguments ------------------------

-- Inititates the game state with the user entered arguments
initGameState :: [String] -> Either String GameState
initGameState args
  | null args = Left "[INFO] No command line arguments were detected" -- If no arguments were entered, return an error message
  | length args /= 4 =
    Left ("[ERROR] An invalid number of arguments were entered (" ++ show (length args) ++ ")" ++ usg) -- If invalid number of arguments entered then reurn another meaningful message
  | otherwise = 
    case (size, ai, lv, asp) of
      (Just sz, Just ai, Just lv, Just asp) -> Right (GameState (initBoard sz asp) undefined False ai lv (other ai))
      (_, _, _, _) -> Left ("[ERROR] Could not initialise board with given arguments" ++ usg)
    where
      size = getSize (head args)   -- Size is first argument (head) of arguments list
      ai = getColour (args !! 1)   -- AI colour is second argument 
      lv = getLevel (args !! 2)    -- AI Level is third argument 
      asp = getASP (args !! 3)     -- Alternative Starting Positions is fourth argument 


-- Parses the entered board size
getSize :: String -> Maybe Int 
getSize size =
  case readMaybe size :: Maybe Int of
    Just x ->
      if x > 3 && x < 27 && (x `mod` 2 == 0)
        then Just x
        else Nothing
    Nothing -> Nothing


-- Parses entered AI Level
getLevel :: String -> Maybe Int
getLevel level = 
  case readMaybe level :: Maybe Int of
    Just x ->
      if x < 4 && x > 0
        then Just x
        else Nothing
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


-- Usage message. Helps to main readability by making it its own function
usg :: String
usg = "\n[USAGE] :main <Board Size> <AI Colour> <AI Level> <ASP Enabled>" ++
      "\n\t- Board Size:  (Int)  Range 4-26. Must be an even number." ++
      "\n\t- AI Colour:   (Col)  Black or White only." ++
      "\n\t- AI Level:    (Int)  Range 1-3 only" ++
      "\n\t- ASP Enabled: (Bool) True or False Only."