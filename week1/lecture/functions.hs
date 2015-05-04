sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n -1)


hailstone :: Integer -> Integer
hailstone n 
	| mod n 2 == 0 = div n 2
	| otherwise 	 = (3 * n) + 1  
