x :: Int
x = 3

y :: Int
y = y + 1 -- this seems to block when you resolve it?

-- int has upper and lower bounds
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

-- Integer is only limited by machine memory (interesting!)
reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

s :: String
s = "Hello Haskell"

noteq = (16 /= 3)

-- if statements not used very often, but they do exist. Pattern matching/guards instead
