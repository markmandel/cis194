module AlgabraicDataTypes where

-- Data type with multiple constructor options. Enum type
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- write function by pattern matching
isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

-- better pattern matching
isSmall2 :: Thing -> Bool
isSmall2 Shoe = True
isSmall2 Cabbage = True
isSmall2 SealingWax = True
isSmall2 _ = False

-- beyond enumerations
data FailableDouble = Failure
                    | OK Double
    deriving Show

ex01 = Failure
ex02 = OK 0.34

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

--- Person
data Person = Person String Int Thing
    deriving Show

richard = Person "richard" 32 Ship
stan = Person "Stan" 15 King

getAge ::Person -> Int
getAge (Person _ x _) = x

-- get the whole p with the pattern matched data
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ Ship) =  n ++ ": Best thing ever"
checkFav (Person n _ _) = n ++ ": Whatevs."

-- use case expressions
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d -> d

-- polymorphic data types

example_a :: Maybe Int -> Int
example_a (Just n) = n
example_a Nothing = -1

data LogMessage = LogMessage Int String

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity string) | severity >= 50 = Just string
example_b _ = Nothing