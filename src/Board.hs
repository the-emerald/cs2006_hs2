module Board where


data Col = Black | White
  deriving Show

-- Define Eq for colours to allow for == to be used
instance Eq Col where 
  (==) Black Black = True 
  (==) White White = True 
  (==) _ _ = False


-- Swaps colour  
other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show


-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard :: Int -> Board
initBoard size = Board size 0 (getInitialPieces size)


-- Gets the position of the first four starter pieces based on the board size 
getInitialPieces :: Int -> [(Position, Col)]
getInitialPieces size =  do let a = (size - 1) `div` 2
                            let b = a + 1
                            [ ((a , a), White), ((b, a), Black), 
                              ((a, b), Black),  ((b, b), White) ]


-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, which is the computer player, timers, information about 
-- rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data GameState 
       = GameState { board :: Board,
                     turn :: Col }



-- Gets the initial game state
-- Size of the board is passed in to allow initial pieces to be positioned correctly
initGameState :: Int -> GameState
initGameState size = GameState (initBoard size)  Black



-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = case checkMove board colour position of
                                        True -> do let pieces' = map (\x -> flipPiece (getFlipList board colour position) x) (pieces board)
                                                   Just (Board (size board) 0 (addPiece (pieces') (position, colour))) 
                                        
                                        False -> Nothing



-- Combines all of the checks into one, declutters makeMove method
checkMove :: Board -> Col -> Position -> Bool
checkMove board colour position = positionOnBoard board position 
                                  && cellEmpty (pieces board) position
                                  && length (getFlipList board colour position) > 0



-- Checks if entered position is on the current board
positionOnBoard :: Board -> Position -> Bool
positionOnBoard board (x,y)
      | x == -1 || y == -1 = False                        -- -1 Signals error in input (See Input.hs) therefore position must be invalid
      | x >= (size board) || y >= (size board) = False    -- If either the x or y value is greater than the size of the current board then return false
      | x < 0 || y < 0 = False                            -- If either the x or y valud is less than the minimum possible positional value, return false 
      | otherwise = True                                  -- Otherwise the entered position must be on the board



-- Checks that cell is empty where piece is being placed
cellEmpty :: [(Position, Col)]  -> Position -> Bool
cellEmpty [] position = True                              -- If end of list is reached and no match has been found the cell must be free
cellEmpty (((x,y), player) : xs) (xPos, yPos)             
      | xPos == x && yPos == y = False                    -- If xPos and yPos matches then cell is populated and therefore piece cannot be placed
      | otherwise = cellEmpty xs (xPos, yPos)             -- Keep searching through list to check for match



-- Gets a list of pieces to be flipped in all 8 directions 
getFlipList :: Board -> Col -> Position -> [Position]
getFlipList board colour position  = do let directions = [(0,-1), (1,0), (0,1), (-1,0), (1,-1), (1,1), (-1,1), (-1,-1)]   -- List representing all possible directions in which tokens could be flipped
                                        concat (map (\x -> getFlipsForDirection board colour position x []) directions)   -- Generates a list of all flippable pieces in all directions
                                          


-- Gets a list of pieces to be flipped in a single direction
getFlipsForDirection :: Board -> Col -> Position -> (Int,Int) -> [Position] -> [Position]
getFlipsForDirection board colour (x,y) (nextX, nextY) xs = do let x' = x + nextX
                                                               let y' = y + nextY
                                                                
                                                               if cellEmpty (pieces board) (x', y') == False then                               -- If the next cell is not empty 
                                                                   if (getCellColour (x',y') (pieces board) /= colour) then                     -- If the cell is of a different colour 
                                                                       getFlipsForDirection board colour (x',y') (nextX,nextY) ((x',y') : xs)   -- Add the position to the list of positions to be flipped
                                                                     else                                                                       -- If the cell is the same colour then all flippable pieces have been found
                                                                       xs                                                                       -- Either an empty list will be returned or the previously discovered pieces of different colour
                                                                 else                                                                           -- If the next cell is empty then return empty list
                                                                   []



-- Changes colour of piece if it is present in the list of pieces to be flipped
flipPiece :: [Position] -> (Position, Col) -> (Position, Col)
flipPiece pieces (position, colour) = do let toFlip = (filter (\x -> x == position) pieces) == [position]   -- Determines the current piece is present in the list of pieces to flip
                                         
                                         if toFlip then                   -- If the piece is present
                                            (position, (other colour))    -- Flip colour and return piece
                                          else                            -- If not present 
                                            (position, colour)            -- return piece unchanged



-- Adds Piece to given list of pieces 
addPiece :: [(Position, Col)] -> (Position, Col) -> [(Position, Col)]
addPiece pieces piece = pieces ++ [piece] 



 -- Given a row and an index return the colour of piece                               -- <<< NOT VALIDATED
getCellColour :: Position -> [(Position, Col)] -> Col                                                    
getCellColour (xPos, yPos) (((x,y), colour) : xs) = if xPos == x && yPos == y then    
                                                        colour           
                                                      else 
                                                        getCellColour (xPos, yPos) xs       



-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore board = do let blackCount = length (filter (\x -> snd x == Black) (pieces board))     -- Get count for black pieces
                      let whiteCount = length (filter (\x -> snd x == White) (pieces board))     -- Get count for white pieces 
                      (blackCount, whiteCount)                                                   -- Combine counts into (int, int) tuple



-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board 
      | length (pieces board) == ((size board) * (size board)) = True     -- If the number of pieces placed is equal to the size of the board then game is over
      | (passes board) == 2 = True                                        -- If the number of passes reaches two then game is over
      | otherwise = False                                                 -- Otherwise the game is not over



-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
