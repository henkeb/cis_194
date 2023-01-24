module Main where

import qualified LogAnalysis (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  LogAnalysis.someFunc
