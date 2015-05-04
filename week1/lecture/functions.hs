sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n -1)


-- extra guards
hailstone :: Integer -> Integer
hailstone n 
	| mod n 2 == 0 = div n 2
	| otherwise 	 = (3 * n) + 1
	
-- you can also mix and match guard statements
foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3
  
isEven :: Integer -> Bool
isEven n = mod n 2 == 0
