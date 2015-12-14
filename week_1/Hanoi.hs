module Hanoi
( hanoi
) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n pa pb pc
  | n == 0 = []
  | otherwise =
      hanoi (n-1) pa pc pb ++ [(pa, pb)] ++ hanoi (n-1) pc pb pa


-- not finished
hanoi_4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi n pa pb pc pd = []
