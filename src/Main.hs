module Main where

import Board
import Display
import Input
import AI

gameLoop :: GameState -> IO ()
gameLoop st
    = do putStr "Move: "
         move <- getLine
         let (x, y) = getCoord move
         let newBoard = makeMove (board st) (turn st) (x, y)

         case newBoard of 
            Just board -> do let st' = GameState board (other (turn st))
                             putStrLn( showGameState st' )
                             gameLoop st'

            Nothing -> do putStrLn "[ERROR] Invalid Move"
                          gameLoop st
         

main :: IO ()
main = do let st = initGameState 8
          putStrLn (showGameState st) 
          gameLoop (st)
