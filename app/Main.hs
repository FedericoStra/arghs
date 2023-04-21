module Main where

import Arghs
import Data.Foldable (traverse_)

main :: IO ()
main = do
  args <- getAllArgs
  let width = lenToWidth $ length args
  traverse_ (printArg width) (enumerate args)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0 ..]
