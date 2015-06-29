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
sumTo20 nums = go 0 nums -- the accumulator, starting at 0
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
