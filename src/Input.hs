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
         | snd (getCoord input) == -1 = Option    -- If move parsing failed for y value then assume the user has entered an option such as 'settings'
         | fst (getCoord input) == -1 = Option    -- If move parsing failed for x value then assume the user has entered an option such as 'settings'
         | otherwise = Move                       -- Otherwise parsing was succesfull and the move should be validated and played


-- Converts uppercase characters in a string to lower case characters
-- This allows the users to input both uppercase and lowercase characters without the program
-- invalidating their input. 
lowerStr :: String -> String 
lowerStr input = map toLower input