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

-- fold
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

fold :: (a -> b -> b) -> [a] -> b -> b
fold f [] z = z
fold f (x:xs) z = f x (fold f xs z)

length'' :: [a] -> Int
length'' xs  = fold addOne xs 0 where
                addOne _ x = 1 + x

-- it's a comp operation. Although does RHS first?
add1Mult4 :: [Int] -> [Int]
add1Mult4 x = map ((*4) . (+1)) x

-- $ is basically ->
negateNumEvens1 :: [Int] -> Int
negateNumEvens1 x = negate (length (filter even x))

negateNumEvens2 :: [Int] -> Int
negateNumEvens2 x = negate $ length $ filter even x

-- Anonymous functions
duplicate1 :: [String] -> [String]
duplicate1 = map dup
               where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

--Currying
mult' :: (Int,Int) -> Int
mult' (x, y) = x * y
