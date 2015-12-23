module Golf where

import Data.List


-- skips "Hello!" = skipsWithNum [(1, 'H'), (2, 'e'), (3, 'l'), (4, 'l'), (5, 'o'), (6, '!')] 6
skipsWithNum :: [(Int, a)] -> Int -> [[a]]
skipsWithNum l n
  | n == 0 = []
  | otherwise = skipsWithNum l (n-1) ++
                [map (\(_, v) -> v) $ filter (\(idx, _) -> idx `mod` n == 0) l]

skips :: [a] -> [[a]]
skips l = skipsWithNum (zip [1..] l) (length l)


localMaxima :: [Integer] -> [Integer]
localMaxima l
  | length l < 3 = []
  | otherwise = if a < b && c < b then b:localMaxima (b:c:rest) else localMaxima (b:c:rest)
    where (a:b:c:rest) = l


-- Execise 3 Histogram
--
-- BAD Solution.

-- calculate the count of Integer n in Integer List l
count :: [Integer] -> Integer -> Int
count [] _ = 0
count (x:xs) n = (if x == n then 1 else 0) + count xs n

-- calculate the count of each 0-9 in Integer List l
stat :: [Integer] -> [Int]
stat l = map (count l) [0..9]

-- represent stat result with `space` and `star`
convert :: [Int] -> [String]
convert l = transpose $ map (\n -> (replicate (mn - n) ' ') ++ (replicate n '*')) l
  where mn = maximum l

histogram :: [Integer] -> String
histogram l = intercalate "\n" $ (convert . stat $ l) ++ [replicate 10 '=', ['0'..'9']]
