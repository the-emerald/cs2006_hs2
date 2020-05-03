module AI where

import Board
import Data.List (maximumBy)
import Control.Parallel.Strategies

import System.Random (randomRIO)
import Control.Monad.Trans.Class


data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c =
  let moves = gen b c in -- generated moves
    GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Generate some set of "good" moves given the Board and Col
generateMove :: Board -> Col -> [Position]
generateMove b c =
  filter (\x -> case makeMove b c x of
    Just ok -> abs (evaluate ok c) > 0
    Nothing -> False) (getValidMoves b c)


-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove _ (GameTree _ _ []) = error "It's empty, yo!"
getBestMove md (GameTree bd cl nxs) = fst (maximumBy (\x y -> compare (minimax md x) (minimax md y)) nxs)
  where
    minimax :: Int -> (Position, GameTree) -> Int
    minimax 0 (_, GameTree b c _) = evaluate b c
    minimax _ (_, GameTree b c []) = evaluate b c
    minimax ply (_, GameTree b c ts) = maximum (parMap rdeepseq (minimax (ply - 1)) ts)

-- Update the world state after some time has passed
updateGameState :: GameState -- ^ current game state
                   -> GameState -- ^ new game state after computer move
updateGameState w =
  case (aiLevel w) of
    1 -> randomMove w
    2 -> easyAI w
    3 -> hardAI w

randomMove :: GameState -> GameState
--randomMove st = case makeMove (board st) (ai st) randomMove of
--                  Just ok -> GameState ok st (canUndo st) (ai st) (aiLevel st) (other (turn st))
--                  Nothing -> error "Random move generator made an illegal move"
--                where 
--                  mvs = getValidMoves (board st) (ai st)
--                  randomMove = lift (fmap (mvs !!) (randomRIO (0, length mvs - 1)))
randomMove = undefined -- NEEDS TO BE FIXED


easyAI :: GameState -> GameState
easyAI st = undefined

hardAI :: GameState -> GameState
hardAI st = case makeMove (board st) (ai st) aiMove of
              Just ok -> GameState ok st (canUndo st) (ai st) (aiLevel st) (other (turn st))
              Nothing -> error "AI made an illegal move"
            where
              gt = buildTree generateMove (board st) (ai st)
              aiMove = getBestMove 5 gt -- Search up to 5 plys



{- Hint: 'updateGameState' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateGameState should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateGameState' should also check if either 
 player has won and display a message if so.
-}


