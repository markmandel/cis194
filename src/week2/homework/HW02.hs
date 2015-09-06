{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List
{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches x y = length $ filter (\(a:b:_) -> a == b) $ transpose [x,y]

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map f colors where
                  f x = length $ filter (\y -> x == y) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = length $ intersect x y

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
-- first code is the secret, and the 2nd is the actual guess
getMove :: Code -> Code -> Move
getMove secret guess = (Move guess matchCount (matches secret guess - matchCount)) where
                        matchCount = exactMatches secret guess

-- Exercise 4 -----------------------------------------

{-
if the guess
inside the Move has the same number of exact and non-exact matches
with the provided Code as it did with the actual secret, then the Code
is consistent with the Move
-}

isConsistent :: Move -> Code -> Bool
isConsistent move code = isMovesConsistent move (getConsistentMove move code)
    where
    isMovesConsistent (Move _ exact1 regular1) (Move _ exact2 regular2) = (exact1 == exact2) && (regular1 == regular2)
    getConsistentMove (Move guess _ _) ccode = getMove ccode guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
