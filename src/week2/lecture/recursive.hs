-- example of a recursive data structure
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show


tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 3 (Leaf 'u'))