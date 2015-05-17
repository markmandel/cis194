-- simple enumeration type
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

-- List of enumeration typess
listOThings :: [Thing]
listOThings = [Shoe, Ship, King, SealingWax]

-- Type casing in a function
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True
