module Display where

import Board
import Data.Char
import Data.Ix


-- Given a game state, return a String which represents the state of the
-- board.
showGameState :: GameState -> String
showGameState g = "\n" ++ headSpace (board g) ++ getHeader (size (board g)) ++ "\n" ++ getTable (board g) 0


-- Returns letters for top of board
getHeader :: Int -> String
getHeader size = do let xs = [65 .. 65 + (size - 1)]                   
                    addSpaces(map chr xs)


-- Gets the correct space for header based on board size 
headSpace :: Board -> String 
headSpace board 
        | (size board) > 9 = "   "   -- 9 is the last possible single digit number
        | otherwise = "  "


-- Adds spaces between each element in a list of characters (String)
addSpaces :: String -> String
addSpaces [] = []
addSpaces (x:xs) =  x : ' ' : addSpaces xs


-- Gets string representation of each row of the table 
getTable :: Board -> Int -> String
getTable board row = if (row < (size board)) then 
                        getTableRow board (range( (0, row), (size board - 1, row) )) ++ "\n" ++ getTable board (row + 1)
                      else 
                        ""


-- Returns pieces on given row of board 
getTableRow :: Board -> [(Int, Int)] -> String
getTableRow board [] = []                                                                                                   
getTableRow board ((xPos, yPos) : xs)                                                                                       
        | xPos == 0 = if ( (size board) > 9 && (yPos + 1) < 10 ) then
                         show(yPos + 1) ++ "  " ++ getCell (xPos, yPos) (pieces board) : ' ' : getTableRow board xs 
                       else 
                         show(yPos + 1) ++ ' ' : getCell (xPos, yPos) (pieces board) : ' ' : getTableRow board xs 
        
        | otherwise = getCell (xPos, yPos) (pieces board) : ' ' : getTableRow board xs                                    


-- Given a row and an index return either . B W
getCell :: Position -> [(Position, Col)] -> Char
getCell cell [] = '.'                                                           -- If function reaches end of list without finding match, space on board must be empty 
getCell (xPos, yPos) (((x,y), player) : xs) = if xPos == x && yPos == y then    -- If match is found 
                                                case player of 
                                                  Black -> 'B'                  -- If the player colour is black return black piece
                                                  White -> 'W'                  -- If the player colour is white return white piece 
                                               else 
                                                  getCell (xPos, yPos) xs       -- Keep searching until end of list for a matching piece