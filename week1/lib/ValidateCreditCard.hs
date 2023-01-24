module ValidateCreditCard where

-- Validating credit card

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : (y : zs)) = x * 2 : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x : xs) = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n
    | n `mod` 10 == 0 = True
    | otherwise = False
