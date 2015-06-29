-- simple enumeration type
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

-- Person with multiple argument constructor
data Person = Person String Int Thing
  deriving Show

-- sample people
brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 15 King

-- using pattern matching to get the age out
getAge :: Person -> Int
getAge (Person _ age _) = age

