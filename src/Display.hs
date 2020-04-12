module Display where

import Board
import Data.Char
import Data.Ix

{- Displaying the board. For example, you could render it as follows,
 - where 'O' is a white piece and '*' is a black piece:
 
   A B C D E F G H
 1 . . . . . . . .
 2 . . . . . . . .
 3 . . . . . . . .
 4 . . . O * . . .
 5 . . . * O . . .
 6 . . . . . . . .
 7 . . . . . . . .
 8 . . . . . . . .
 
 -}

-- Given a game state, return a String which represents the state of the
-- board.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
showGameState :: GameState -> String
showGameState g = undefined


-- Returns letters for top of board
getHeader :: Int -> String
getHeader size = do let xs = [65 .. 65 + (size - 1)]                   
                    addSpaces(map chr xs)


-- Adds spaces between each element in a list of characters (String)
addSpaces :: String -> String
addSpaces [] = []
addSpaces (x:xs) =  x : ' ' : addSpaces xs


-- Returns pieces on given row of board 
getTableRow :: Board -> [(Int, Int)] -> String
getTableRow board [] = []
getTableRow board (cell : xs) = getCell cell (pieces board) : getTableRow board xs
  

-- Given a row and an index return either . * O 
getCell :: (Int,Int) -> [(Position, Col)] -> Char
getCell cell [] = '.'                                                         -- If function reaches end of list without finding match, space on board must be empty 
getCell (xPos, yPos) (((x,y), player) : xs) = if xPos == x && yPos == y then  -- If match is found 
                                              case player of 
                                                Black -> '*'                  -- If the player colour is black return black piece
                                                White -> 'O'                  -- If the player colour is white return white piece 
                                            else 
                                                getCell (xPos, yPos) xs       -- Keep searching until end of list for a matching piece
