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