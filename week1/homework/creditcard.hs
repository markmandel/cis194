
doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x:[]) = [x]
doubleSecond(x:(y:rest)) = x : (2 * y) : doubleSecond rest
