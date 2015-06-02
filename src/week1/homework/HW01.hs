{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n
    | n < 10 = n
    | otherwise = n - ((n `div` 10) * 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - (lastDigit n)) `div` 10


-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0 = []
    | otherwise = lastDigit n : toRevDigits (dropLastDigit n)


toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the right (so you need to reverse it).
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther(x:(y:rest)) = x : (2 * y) : doubleEveryOther rest


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits n = sum (concatMap toDigits n)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (mod (sumDigits (reverse (doubleEveryOther (toRevDigits n)))) 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

{-
 public void solve(int n, String start, String auxiliary, String end) {
       if (n == 1) {
           System.out.println(start + " -> " + end);
       } else {
           solve(n - 1, start, end, auxiliary);
           System.out.println(start + " -> " + end);
           solve(n - 1, auxiliary, start, end);
       }
   }
Also referenced : https://github.com/sjakobi/CIS194/blob/master/hw1/TowersOfHanoi.hs

These type of recursive problems are hard :/.
-}

-- disks, 3 pegs. Returns a set of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a