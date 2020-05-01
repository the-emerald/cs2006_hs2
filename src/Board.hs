module Board where

import Data.Ix
import Data.Array.IArray

-- Piece Colours (Either black or white only)
data Col = Black | White
  deriving Show


-- Define Eq for colours to allow for == to be used
instance Eq Col where 
  (==) Black Black = True 
  (==) White White = True 
  (==) _ _ = False


-- Swaps piece colour
other :: Col -> Col
other Black = White
other White = Black


type Position = (Int, Int)


-- A Board is a record containing the board size (a board is a square grid, n * n),
-- Whether or not the first two moves can be at any position on the board (asp)
-- the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.
data Board = Board { size :: Int,
                     asp :: Bool,                     
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show


-- Initial board of specified size, neither player has passed, 4 initial pieces are placed 
-- and there is no possibility for alternative starting positions 
initBoard :: Int -> Bool -> Board
initBoard size asp = Board size asp 0 (getInitialPieces size)


-- Changes 'alternative starting positions' value and returns new board
otherAsp :: Board -> Board
otherAsp board = initBoard (size board) (not(asp board))


-- Gets the position of the first four starter pieces based on the board size 
getInitialPieces :: Int -> [(Position, Col)]
getInitialPieces size =  do let a = (size - 1) `div` 2
                            let b = a + 1
                            [ ((a , a), White), ((b, a), Black), 
                              ((a, b), Black),  ((b, b), White) ]


-- Game State represents the entire game world. Has all of the relevant information for the game to be played
data GameState 
       = GameState { board :: Board,          -- Board information: Size, Passes, Pieces
                     previous :: GameState,   -- Previous game state (used when undoing a move)
                     canUndo :: Bool,         -- Used to determine whether or not a move can be undone 
                     ai :: Col,               -- Which color is being played by the AI
                     turn :: Col }            -- Colour of player whos turn it is
  deriving Show


-- Default Game State
-- This consists of the default 8x8 board, no previous move, no possibility of undoing a move, 
-- white as the ai player and black as the starting turn 
defaultGameState :: GameState
defaultGameState = GameState (initBoard 8 False) undefined False White Black



-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = case checkMove board colour position of
                                        True -> do let pieces' = map (\x -> flipPiece (getFlipList board colour position) x) (pieces board)     -- Gets all pieces and flipped pieces for current move
                                                   Just (Board (size board) (asp board) 0 (addPiece (pieces') (position, colour)))              -- Returns new board: Retains board size, resets passes to 0 and updates pieces on board 
                                        
                                        False -> Nothing                                                                                        -- If the move is invalid then return nothing to signal error



-- Combines all of the checks into one, declutters makeMove method
checkMove :: Board -> Col -> Position -> Bool
checkMove board colour position 
      -- Allows for alternative starting positions
      | (asp board) && (length (pieces board)) < 6 = positionOnBoard board position         -- If alternative starting positions are enabled, if less than 6 pieces are on the board (4 starting + 2 first moves)        
                                                     && cellEmpty (pieces board) position   -- Check that the piece is on the board and that the cell is empty
      -- Standard checks which ensure that pieces are flipped
      | otherwise = positionOnBoard board position                      -- For normal moves: If the position is on the board
                    && cellEmpty (pieces board) position                -- If the cell is empty 
                    && length (getFlipList board colour position) > 0   -- If a piece is flipped. Move is valid



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



-- Given a row and an index return the colour of piece                     
-- This method is only used on non-empty cells. It should never reach the end of the list without finding a match      
getCellColour :: Position -> [(Position, Col)] -> Col        
getCellColour position [] = error "\n[INFO] Reached end of list without match. Ensure position validated (not empty) before searching for colour\n"                                           
getCellColour (xPos, yPos) (((x,y), colour) : xs) = if xPos == x && yPos == y then      -- If the current position is equal to entered position 
                                                        colour                          -- Return the colour of the position  
                                                      else 
                                                        getCellColour (xPos, yPos) xs   -- Otherwise keep searching through list for position. 



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



-- Gets all valid moves for a given board and colour
-- Calls validMoves function. Reduces overall clutter when getting valid moves as
-- only the board and colour have to be given
getValidMoves :: Board -> Col -> [Position]
getValidMoves board colour = validMoves board Black (range ((0,0),(7,7)))


-- Gets all valid moves for a given colour and list of positions
validMoves :: Board -> Col -> [Position] -> [Position]
validMoves board colour [] = []
validMoves board colour (x:xs) = case checkMove board colour x of 
                                        False -> validMoves board colour xs
                                        True -> x:(validMoves board colour xs)


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.

-- Sannidhanam, V., & Annamalai, M. (2015). An Analysis of Heuristics in Othello.
-- and https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/
evaluate :: Board -> Col -> Int
evaluate b c =
  (parity * 25)
  where
    values :: Array (Int, Int) Int
    values = listArray ((0, 0), (7, 7)) $ concat [[4, -3, 2, 2, 2, 2, -3, 4],
            [-3, -4, -1, -1, -1, -1, -4, -3],
            [2, -1, 1, 0 ,0, 1, -1, 2],
            [2, -1, 0, 1, 1, 0, -1, 2],
            [2, -1, 0, 1, 1, 0, -1, 2],
            [2, -1, 1, 0 ,0, 1, -1, 2],
            [-3, -4, -1, -1, -1, -1, -4, -3],
            [4, -3, 2, 2, 2, 2, -3, 4]]

    parity = 69 -- replace me
    cornerOccupancy = 69
    cornerCloseness = 69
    mobility = 69
    frontTiles = 69
    d = 69

-- 5.1.1: Coin Parity (p)
evaluateParity :: Board -> Col -> Int
evaluateParity b c =
  100 * (maxP - minP) `div` (maxP + minP)
  where
    maxP = length (filter (\x -> snd x == c) (pieces b))
    minP = length (filter (\x -> snd x == other c) (pieces b))

-- Karti: Front tiles (L93) (f)
evaluateFront :: Board -> Col -> Int
evaluateFront b c = undefined

-- 5.1.2: Mobility (m)
evaluateMobility :: Board -> Col -> Int
evaluateMobility b c=
  if (maxM + minM) /= 0
    then 100 * (maxM - minM) `div` (maxM + minM)
    else 0
  where
    maxM = length (getValidMoves b c)
    minM = length (getValidMoves b (other c))

-- 5.1.3: Corners Captured (c)
evaluateCornersCaptured :: Board -> Col -> Int
evaluateCornersCaptured b c = 25 * (maxC - minC)
  where
    corners = [(0, 0), (0, 7), (7, 0), (7, 7)]
    -- Filter by not empty, and then filter by cell colour
    maxC = length (filter (\x -> getCellColour x (pieces b) == c) (filter (not . cellEmpty (pieces b)) corners))
    minC = length (filter (\x -> getCellColour x (pieces b) == other c) (filter (not . cellEmpty (pieces b)) corners))

-- Karti: Close corners (l)
evaluateCornerCloseness :: Board -> Col -> Int
evaluateCornerCloseness b c = -12 * (maxL - minL)
  where
    -- The head of each sublist is the corner, and the tail is the neighbours
    closeCorners =
      [ [(0, 0), (0, 1), (1, 1), (1, 0)]
      , [(0, 7), (0, 6), (1, 6), (1, 7)]
      , [(7, 0), (7, 1), (6, 1), (6, 0)]
      , [(7, 7), (6, 7), (6, 6), (7, 6)]
      ]
    maxL = length (filter (\x -> getCellColour x (pieces b) == c) (filter (not . cellEmpty (pieces b)) (concatMap tail (filter (not . cellEmpty (pieces b) . head) closeCorners))))
    minL = length (filter (\x -> getCellColour x (pieces b) == other c) (filter (not . cellEmpty (pieces b)) (concatMap tail (filter (not . cellEmpty (pieces b) . head) closeCorners))))

-- 5.1.4: Stability
evaluateStability :: Board -> Col -> Int
evaluateStability = undefined

--evaluateCornerOccupancy :: Board -> Col -> Int
--evaluateCornerOccupancy = undefined
--