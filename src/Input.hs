module Input where

import Data.Char

import Board
import AI

-- Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -> (Int, Int)
getCoord (a:xs) = ((charToPosition a), ((read xs::Int) - 1))

-- Returns correct value for a given uppercase or lowercase character
charToPosition :: Char -> Int
charToPosition char
        | ord char <= 90 && ord char >= 65 = (ord char) - 65    -- If uppercase character entered return appropriate value
        | ord char <= 122 && ord char >= 97 = (ord char) - 97   -- If lowercase character entered return appropriate value 
        | otherwise = -1                                        -- Return -1 to signal error as entered character is not in a valid range