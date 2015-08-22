-- lets put it all toghether
module Wholemeal where

-- aparently this is bad Haskell style
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs


-- we can do the above, as a series of functions
-- to create a single partial function
foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (> 3)

