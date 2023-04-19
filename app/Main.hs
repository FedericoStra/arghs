module Main where

import System.Environment
import Text.Printf

main :: IO ()
main = do
  args <- getAllArgs
  let width = lenToWidth $ length args
  mapM_ (printArg width) (enumerate args)

getAllArgs :: IO [String]
getAllArgs = do
  progName <- getProgName
  args <- getArgs
  return $ progName : args

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

lenToWidth :: Int -> Int
lenToWidth len
  | len <= 10 = 1
  | len <= 100 = 2
  | len <= 1000 = 3
  | len <= 10000 = 4
  | len <= 100000 = 5
  | len <= 1000000 = 6
  | otherwise = 0

printArg :: Int -> (Int, String) -> IO ()
printArg width (i, arg) =
  printf "%*d: `%s`\n" width i arg
