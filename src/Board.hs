module Board where

data Col = Black | White
  deriving Show

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
makeMove board colour position = Just (Board (size board) 0 (addPiece (pieces board) (position, colour)))   -- << NEED TO GREATLY EXPAND


-- Adds Piece to given list of pieces 
addPiece :: [(Position, Col)] -> (Position, Col) -> [(Position, Col)]
addPiece pieces piece = pieces ++ [piece] 


-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore = undefined


-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver = undefined


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
