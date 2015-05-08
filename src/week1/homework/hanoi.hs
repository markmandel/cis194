type Peg = String
type Move = (Peg, Peg)

-- disks, 3 pegs. Returns a set of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = []