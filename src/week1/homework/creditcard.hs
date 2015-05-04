doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x:[]) = [x]
doubleSecond(x:(y:rest)) = x : (2 * y) : doubleSecond rest

doubleSecondLast :: [Integer] -> [Integer]
doubleSecondLast n = reverse (doubleSecond (reverse n))

listDigits :: Integer -> [Integer]
listDigits 0 = []
listDigits n = rhsDigit n : listDigits (remainingDigits n (rhsDigit n))


-- remaining digits. Original, RHSDigit, returns the result
remainingDigits :: Integer -> Integer -> Integer
remainingDigits original rhs = (original - rhs) `div` 10

-- result is digit, remaining
rhsDigit :: Integer -> Integer
rhsDigit n 
	| n < 10 = n
	| otherwise = n - ((n `div` 10) * 10)
	
