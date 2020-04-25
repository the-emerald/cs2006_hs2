module GameOptions where

import Board

optionHandler :: String -> GameState -> Either String GameState
optionHandler option st = case option of 
                            "pass" -> Right (playerPass st)
                            _ -> Left "[ERROR] Invalid Input"


playerPass :: GameState -> GameState
playerPass st = do let board' = Board (size (board st)) ((passes (board st)) + 1) (pieces (board st))
                   GameState board' (ai st) (other (turn st))