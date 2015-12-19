module Hanoi
( hanoi
) where

type Peg = String
type Move = (Peg, Peg)

-- move from a to b
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n pa pb pc
  | n == 0 = []
  | otherwise =
      hanoi (n-1) pa pc pb ++ [(pa, pb)] ++ hanoi (n-1) pc pb pa


-- Reve's puzzle (Hanoi with 4 pegs)
--
-- move from a to b
-- 1. move k from a to c
-- 2. move n - k from a to b without using c
-- 4. move k from c to b
hanoi_4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi_4 n pa pb pc pd
  | n == 0 = []
  | n == 1 = [(pa, pb)]
  | otherwise =
     hanoi_4 k pa pc pb pd ++ hanoi (n-k) pa pb pd ++
     hanoi_4 k pc pb pa pd
     where k = n `quot` 2
     -- where k = (toInteger . floor . sqrt . fromIntegral $ (2 * n + 1)) - 1
