module Input where

import Data.Char

import GameOptions
import Move
import Board
import AI

-- Define Input Types 
data InputType = Move | Option
  deriving Show


-- Define Eq for InputType to allow for == to be used
instance Eq InputType where 
  (==) Move Move = True 
  (==) Option Option = True 
  (==) _ _ = False


-- Given a state, get input from getLine and return new state or user message
nextState :: String -> GameState -> Either String GameState
nextState input st = do if inputType input == Move then 
                          moveHandler input st
                         else
                          optionHandler input st


-- Given an input string, get the type of the input
inputType :: String -> InputType
inputType input  
         | snd (getCoord input) == -1 = Option
         | fst (getCoord input) == -1 = Option
         | otherwise = Move


-- Converts uppercase characters in a string to lower case characters
lowerStr :: String -> String 
lowerStr input = map toLower input