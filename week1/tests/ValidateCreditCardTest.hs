module Main where

import qualified System.Exit as Exit
import Test.HUnit
import ValidateCreditCard

testToDigits :: Test
testToDigits = TestCase (assertEqual "should return [1, 2]" [1, 2, 3, 4] (toDigits 1234))

testToDigitsZero :: Test
testToDigitsZero = TestCase (assertEqual "should return [1, 2]" [] (toDigits 0))

testToDigitsNegative :: Test
testToDigitsNegative = TestCase (assertEqual "should return [1, 2]" [] (toDigits 0))

testToDigitsRev :: Test
testToDigitsRev = TestCase (assertEqual "should return [4, 3, 2, 1]" [4, 3, 2, 1] (toDigitsRev 1234))

testDoubleEverOtherEven :: Test
testDoubleEverOtherEven = TestCase (assertEqual "should return [16, 7, 12, 5]" [16, 7, 12, 5] (doubleEveryOther [8, 7, 6, 5]))

tests :: Test
tests = TestList [TestLabel "toDigits" testToDigits, TestLabel "toDigitsZero" testToDigitsZero, TestLabel "toDigitsNegative" testToDigitsNegative, TestLabel "toDigitsRev" testToDigitsRev, TestLabel "doubleEveryOtherEven" testDoubleEverOtherEven]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
