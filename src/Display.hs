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
showGameState g = "  " ++ getHeader (size (board g)) ++ "\n" ++ getTable (board g) (size (board g))


-- Returns letters for top of board
getHeader :: Int -> String
getHeader size = do let xs = [65 .. 65 + (size - 1)]                   
                    addSpaces(map chr xs)


-- Adds spaces between each element in a list of characters (String)
addSpaces :: String -> String
addSpaces [] = []
addSpaces (x:xs) =  x : ' ' : addSpaces xs


getTable :: Board -> Int -> String
getTable board row = if (row > 0) then 
                        getTableRow board (range( (0, row -1), (size board - 1, row - 1) )) ++ "\n" ++ getTable board (row - 1)
                      else 
                        ""

-- Returns pieces on given row of board 
getTableRow :: Board -> [(Int, Int)] -> String
getTableRow board [] = []                                                                                                   -- If end of list detected, output empty list
getTableRow board ((xPos, yPos) : xs)                                                                                       -- Given a board and a range of coordinates 
        | xPos == 0 = show((size board) - yPos) ++ ' ' : getCell (xPos, yPos) (pieces board) : ' ' : getTableRow board xs   -- If current coordinate is the first in a column (x = 0) then print the column number 
        | otherwise = getCell (xPos, yPos) (pieces board) : ' ' : getTableRow board xs                                      -- Otherwise print the next coordinates piece 


-- Given a row and an index return either . * O 
getCell :: (Int,Int) -> [(Position, Col)] -> Char
getCell cell [] = '.'                                                         -- If function reaches end of list without finding match, space on board must be empty 
getCell (xPos, yPos) (((x,y), player) : xs) = if xPos == x && yPos == y then  -- If match is found 
                                              case player of 
                                                Black -> '*'                  -- If the player colour is black return black piece
                                                White -> 'O'                  -- If the player colour is white return white piece 
                                            else 
                                                getCell (xPos, yPos) xs       -- Keep searching until end of list for a matching piece