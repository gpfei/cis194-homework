module Credit
( validate
) where

-- toDigits :: Integer -> [Integer]
-- toDigits n
--   | n <= 0 = []
--   | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


validate :: Integer -> Bool
validate n = check == 0
   where check = (sumDigits . doubleEveryOther . toDigitsRev $ n) `mod` 10
