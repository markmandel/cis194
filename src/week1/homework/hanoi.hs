type Peg = String
type Move = (Peg, Peg)

{-
 public void solve(int n, String start, String auxiliary, String end) {
       if (n == 1) {
           System.out.println(start + " -> " + end);
       } else {
           solve(n - 1, start, end, auxiliary);
           System.out.println(start + " -> " + end);
           solve(n - 1, auxiliary, start, end);
       }
   }
-}

-- disks, 3 pegs. Returns a set of moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start aux end
    | n <= 1 = [(start, end)]
    | otherwise = concat [hanoi (n - 1) start end aux, [(start, end)], hanoi (n - 1) aux start end]