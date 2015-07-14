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

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

exampleList = [-1, 2, 6]::[Int]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x = x : myFilter p xs
  | otherwise = myFilter p xs

evenNumbers :: [Int] -> [Int]
evenNumbers = myFilter f where
                f :: Int -> Bool
                f x = mod x 2 == 0