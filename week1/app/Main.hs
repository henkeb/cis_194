module Main where

import ValidateCreditCard

-- Towers of Hanoi
-- https://commandercoriander.net/blog/2016/01/05/solving-the-towers-of-hanoi-with-haskell/
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end temp = hanoi (n - 1) start temp end ++ [(start, end)] ++ hanoi (n - 1) temp end start

main :: IO ()
main = do
    -- should print True
    print $ validate $ sumDigits $ doubleEveryOther $ toDigits 4012888888881881
    -- should print False
    print $ validate $ sumDigits $ doubleEveryOther $ toDigits 4012888888881882
    print $ hanoi 2 "left" "right" "mid"
