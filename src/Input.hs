module Input where

import Data.Char

import Board
import AI

-- Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -> (Int, Int)
getCoord (a:b:xs) = ((digitToInt a), (digitToInt b))
