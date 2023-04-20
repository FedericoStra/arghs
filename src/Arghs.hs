{-# LANGUAGE Safe #-}

-- |
-- Module      :  Arghs
-- Copyright   :  (c) Federico Stra 2023
-- License     :  MIT (see the file LICENSE in this distribution)
--
-- Maintainer  :  Federico Stra <stra.federico@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple program to list the arguments passed on the command line.
module Arghs (getAllArgs, printArg, lenToWidth) where

import Control.Applicative (liftA2)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

-- | Computation 'getAllArgs' returns a list of the program's command
-- line arguments, including the program name.
getAllArgs :: IO [String]
getAllArgs = liftA2 (:) getProgName getArgs

-- |
-- Computation @'printArg' width (i, arg)@ prints a single command line argument.
--
-- The argument is delimited by backticks and prepended by the index @i@ followed by a colon.
-- The index is formatted with width @width@.
--
-- >>> printArg 3 (8, "hello")
--   8: `hello`
printArg :: Int -> (Int, String) -> IO ()
printArg width (i, arg) = printf "%*d: `%s`\n" width i arg

-- | @'lenToWidth' len@ computes the width needed to print indices for @len@ arguments.
lenToWidth :: Int -> Int
lenToWidth len
  | len <= 10 = 1
  | len <= 100 = 2
  | len <= 1000 = 3
  | len <= 10000 = 4
  | len <= 100000 = 5
  | len <= 1000000 = 6
  | otherwise = 0
