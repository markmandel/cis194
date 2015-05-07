doubleEveryOtherForwards :: [Integer] -> [Integer]
doubleEveryOtherForwards [] = []
doubleEveryOtherForwards (x:[]) = [x]
doubleEveryOtherForwards(x:(y:rest)) = x : (2 * y) : doubleEveryOtherForwards rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherForwards (reverse n))

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = rhsDigit n : toDigitsRev (remainingDigits n (rhsDigit n))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- remaining digits. Original, RHSDigit, returns the result
remainingDigits :: Integer -> Integer -> Integer
remainingDigits original rhs = (original - rhs) `div` 10

-- result is digit, remaining
rhsDigit :: Integer -> Integer
rhsDigit n
    | n < 10 = n
    | otherwise = n - ((n `div` 10) * 10)

-- sum up all the digits
sumDigits :: [Integer] -> Integer
sumDigits n = sum (concatMap toDigits n)

-- validate the checksome
validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0