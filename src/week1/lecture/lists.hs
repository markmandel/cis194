range :: [Integer]
range = [1..100]

-- cons operator
onetwothree :: [Integer]
onetwothree = 1 : 2 : 3 : []

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
  
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- pattern matching on lists
intListLen :: [Integer] -> Integer
intListLen [] = 0
-- similar pattern matchin gto tuples. Interesting!
intListLen (x:xs) = 1 + intListLen xs

-- pattern matching can be nested, which is super neat
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = [] -- who cares
sumEveryTwo (x:[]) = [x] -- ignore single elements
sumEveryTwo (x:(y:rest)) = x + y : sumEveryTwo rest

