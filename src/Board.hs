module Board where

import Data.Ix
import Data.Array.IArray
import Data.List.NonEmpty (nub)
import Data.List (nubBy)

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
getInitialPieces size =
  [((a , a), White), ((b, a), Black), ((a, b), Black),  ((b, b), White)]
  where
    a = (size - 1) `div` 2
    b = a + 1


-- Game State represents the entire game world. Has all of the relevant information for the game to be played
data GameState 
       = GameState { board :: Board,          -- Board information: Size, Passes, Pieces
                     previous :: GameState,   -- Previous game state (used when undoing a move)
                     canUndo :: Bool,         -- Used to determine whether or not a move can be undone 
                     ai :: Col,               -- Which color is being played by the AI
                     aiLevel :: Int,           -- Detemines which AI implementation is to be used 
                     turn :: Col }            -- Colour of player whos turn it is
  deriving Show


-- Default Game State
-- This consists of the default 8x8 board, no previous move, no possibility of undoing a move, 
-- white as the ai player, random AI (Level 0) and black as the starting turn 
defaultGameState :: GameState
defaultGameState = GameState (initBoard 8 False) undefined False White 1 Black



-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position =
  if checkMove board colour position
    then (do let pieces' = map (flipPiece (getFlipList board colour position)) (pieces board)
             Just (Board (size board) (asp board) 0 (addPiece pieces' (position, colour))))
    else Nothing  -- If the move is invalid then return nothing to signal error



-- Combines all of the checks into one, declutters makeMove method
checkMove :: Board -> Col -> Position -> Bool
checkMove board colour position
      -- Allows for alternative starting positions
  | asp board && length (pieces board) < 6 =
    positionOnBoard board position -- If alternative starting positions are enabled, if less than 6 pieces are on the board (4 starting + 2 first moves)
     &&
    cellEmpty (pieces board) position -- Check that the piece is on the board and that the cell is empty
      -- Standard checks which ensure that pieces are flipped
  | otherwise =
    positionOnBoard board position -- For normal moves: If the position is on the board
     &&
    cellEmpty (pieces board) position -- If the cell is empty
     &&
    not (null (getFlipList board colour position))   -- If a piece is flipped. Move is valid



-- Checks if entered position is on the current board
positionOnBoard :: Board -> Position -> Bool
positionOnBoard board (x, y)
  | x == -1 || y == -1 = False -- -1 Signals error in input (See Input.hs) therefore position must be invalid
  | x >= size board || y >= size board = False -- If either the x or y value is greater than the size of the current board then return false
  | x < 0 || y < 0 = False -- If either the x or y value is less than the minimum possible positional value, return false
  | otherwise = True                                  -- Otherwise the entered position must be on the board



-- Checks that cell is empty where piece is being placed
cellEmpty :: [(Position, Col)]  -> Position -> Bool
cellEmpty [] position = True                              -- If end of list is reached and no match has been found the cell must be free
cellEmpty (((x,y), player) : xs) (xPos, yPos)             
      | xPos == x && yPos == y = False                    -- If xPos and yPos matches then cell is populated and therefore piece cannot be placed
      | otherwise = cellEmpty xs (xPos, yPos)             -- Keep searching through list to check for match



-- Gets a list of pieces to be flipped in all 8 directions 
getFlipList :: Board -> Col -> Position -> [Position]
getFlipList board colour position = 
  concatMap (\x -> getFlipsForDirection board colour position x []) directions   -- Generates a list of all flippable pieces in all directions
  where
    directions = [(0, -1), (1, 0), (0, 1), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)] -- List representing all possible directions in which tokens could be flipped
                                          


-- Gets a list of pieces to be flipped in a single direction
getFlipsForDirection :: Board -> Col -> Position -> (Int,Int) -> [Position] -> [Position]
getFlipsForDirection board colour (x, y) (nX, nY) xs =
  case (nextCellEmpty, cellColour) of
    (True, True) -> getFlipsForDirection board colour (x', y') (nX, nY) ((x', y') : xs)
    (True, False) -> xs
    (False, _) -> []
  where
    x' = x + nX
    y' = y + nY
    nextCellEmpty = not (cellEmpty (pieces board) (x', y'))
    cellColour = getCellColour (x', y') (pieces board) /= colour


-- Changes colour of piece if it is present in the list of pieces to be flipped
flipPiece :: [Position] -> (Position, Col) -> (Position, Col)
flipPiece pieces (position, colour) =
  if toFlip
    then (position, other colour)
    else (position, colour)
  where
    toFlip = filter (== position) pieces == [position]


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


-- Returns winning message
getWinner :: Board -> String
getWinner board 
    | bscore < wscore = "The White player wins!"
    | bscore > wscore = "The Black player wins!"
    | otherwise = "Both players had equal scores!"
  where 
    score = checkScore board
    bscore = fst score
    wscore = snd score


-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board
  | length (pieces board) == (size board * size board) = True -- If the number of pieces placed is equal to the size of the board then game is over
  | passes board == 2 = True -- If the number of passes reaches two then game is over
  | otherwise = False                                                 -- Otherwise the game is not over


-- Pass: If player passes then new state returned with number of passes increased
playerPass :: GameState -> GameState
playerPass st = do
  let board' = Board (size (board st)) (asp (board st)) (passes (board st) + 1) (pieces (board st))
  GameState board' (previous st) True (ai st) (aiLevel st) (other (turn st))


-- Gets all valid moves for a given board and colour
-- Calls validMoves function. Reduces overall clutter when getting valid moves as
-- only the board and colour have to be given
getValidMoves :: Board -> Col -> [Position]
getValidMoves board colour = validMoves board colour (range ((0, 0), (max, max)))
  where
    max = size board - 1


-- Gets all valid moves for a given colour and list of positions
validMoves :: Board -> Col -> [Position] -> [Position]
validMoves board colour [] = []
validMoves board colour (x:xs) =
  if checkMove board colour x
    then x : validMoves board colour xs
    else validMoves board colour xs


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.

-- Sannidhanam, V., & Annamalai, M. (2015). An Analysis of Heuristics in Othello.
-- and https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/
evaluate :: Board -> Col -> Int
evaluate b c =
  (par * 25) + (mob * 5) + (coCap * 15) + (stability * 25) + (coClo * 15) + (front * 5)
  where
    par = evaluateParity b c
    mob = evaluateMobility b c
    coCap = evaluateCornersCaptured b c
    stability = 0
    coClo = evaluateCornerCloseness b c
    front = 0

-- 5.1.1: Coin Parity (p)
evaluateParity :: Board -> Col -> Int
evaluateParity b c =
  100 * (maxP - minP) `div` (maxP + minP)
  where
    maxP = length (filter (\x -> snd x == c) (pieces b))
    minP = length (filter (\x -> snd x == other c) (pieces b))

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
    corners = [(0, 0), (0, size b), (size b, 0), (size b, size b)]
    -- Filter by not empty, and then filter by cell colour
    maxC = length (filter (\x -> getCellColour x (pieces b) == c) (filter (not . cellEmpty (pieces b)) corners))
    minC = length (filter (\x -> getCellColour x (pieces b) == other c) (filter (not . cellEmpty (pieces b)) corners))

-- 5.1.4: Stability
-- TODO: Find a way to calculate stability
evaluateStability :: Board -> Col -> Int
evaluateStability b c = stability
  where
    oc = other c
    unstablePieces =
      length (nubBy (\x y -> fst x == fst y && snd x == snd y) (concatMap (getFlipList b oc) (getValidMoves b oc)))
    stability = 5

-- Karti: Close corners (l)
evaluateCornerCloseness :: Board -> Col -> Int
evaluateCornerCloseness b c = -12 * (maxL - minL)
    -- The head of each sublist is the corner, and the tail is the neighbours
  where
    closeCorners =
      [ [(0, 0), (0, 1), (1, 1), (1, 0)]
      , [(0, size b), (0, size b - 1), (1, size b - 1), (1, size b)]
      , [(size b, 0), (size b, 1), (size b - 1, 1), (size b - 1, 0)]
      , [(size b, size b), (size b - 1, size b), (size b - 1, size b - 1), (size b, size b - 1)]
      ]
    maxL =
      length
        (filter
           (\x -> getCellColour x (pieces b) == c)
           (filter
              (not . cellEmpty (pieces b))
              (concatMap tail (filter (not . cellEmpty (pieces b) . head) closeCorners))))
    minL =
      length
        (filter
           (\x -> getCellColour x (pieces b) == other c)
           (filter
              (not . cellEmpty (pieces b))
              (concatMap tail (filter (not . cellEmpty (pieces b) . head) closeCorners))))

-- Karti: Frontier tiles (L93) (f)
-- TODO: Find a way to calculate frontier tiles
evaluateFront :: Board -> Col -> Int
evaluateFront b c = undefined