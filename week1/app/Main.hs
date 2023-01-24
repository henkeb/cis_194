module Main where

-- Validating credit card
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

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

-- Towers of Hanoi
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end temp = hanoi (n - 1) start temp end ++ [(start, end)] ++ hanoi (n - 1) temp end start

main :: IO ()
main = do
    print $ validate $ sumDigits $ doubleEveryOther $ toDigits 4012888888881881
    print $ validate $ sumDigits $ doubleEveryOther $ toDigits 4012888888881882
    print $ hanoi 2 "left" "right" "mid"
