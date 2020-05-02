module Move where

import Data.Char
import Text.Read

import Board

-- Takes a move and a state and returnes a either new state with the move added, or string message to be displayed
moveHandler :: String -> GameState -> Either String GameState
moveHandler move st = case makeMove (board st) (turn st) (getCoord move) of
                               Nothing -> Left "[Move] Invalid Move"
                               Just board' -> Right (GameState board' st True (ai st) (other (turn st)))


-- Given an input string, convert it to a board coordinate as a pair of Ints
getCoord :: String -> Position
getCoord [] = (-1, -1)
getCoord (x:xs) = (charToPosition x, parseYpos xs)


-- Parses Y Position 
parseYpos :: String -> Int
parseYpos pos =
  case readMaybe pos :: Maybe Int -- Try and parse string as integer
        of
    Just x ->
      if x > 0 && x < 27          -- If the integer is between valid range 1 - 26 then return integer - 1 (Adjusting for starting at 0)
        then x - 1
        else -1                   -- If the number is outside of this range then return -1 to signal error 
    Nothing -> -1                 -- If the entered string cannot be parsed as int then return -1 to signal error


-- Returns position value for a given uppercase or lowercase character
charToPosition :: Char -> Int
charToPosition char
  | ord char <= 122 && ord char >= 97 = ord char - 97 -- If lowercase character entered return appropriate value
  | otherwise = -1                                    -- Return -1 to signal error as entered character is not in a valid range