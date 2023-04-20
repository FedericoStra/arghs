{-# LANGUAGE Safe #-}

module Main where

import Arghs

main :: IO ()
main = do
  args <- getAllArgs
  let width = lenToWidth $ length args
  mapM_ (printArg width) (enumerate args)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0 ..]
