module Variables where

strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength xs in
                    len_rest + 1 --define let_rest, and then add one to it.

frob :: String -> Char
frob [] = 'a' -- len not in scope here.
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str

sumTo20 :: [Int] -> Int
sumTo20 = go 0 -- the accumulator, starting at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc -- empty list, return the accumulated sum
        go acc (x:xs)
          | acc >= 20 = acc
          | otherwise = go (acc + x) xs

{-
bogus :: [a] -> Bool
bogus ('X':_) = True
bogus _ = False
-}

notEmpty :: [a] -> Bool
notEmpty (_:_) = True
notEmpty [] = False


--partial functions are bad. Don't use them
doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [_] = 0
doStuff1 xs = head xs + head (tail xs)

--use pattern matching instead
doStuff2 :: [Int] -> Int
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x:y:_) = x + y

-- implement our own map function

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

exampleList = [-1, 2, 6]::[Int]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

evenNumbers :: [Int] -> [Int]
evenNumbers = filter' f where
                f :: Int -> Bool
                f x = mod x 2 == 0

