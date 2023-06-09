{-# LANGUAGE OverloadedStrings #-}

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
import Formatting (fprintLn, left, string, (%))
import System.Environment (getArgs, getProgName)

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
printArg width (i, arg) = fprintLn (left width ' ' % ": `" % string % "`") i arg

-- | @'lenToWidth' len@ computes the width needed to print indices for @len@ arguments.
lenToWidth :: Int -> Int
lenToWidth len = numDigits (len - 1)

-- | Number of base 10 digits. @n@ must be non-negative.
numDigits :: Int -> Int
numDigits n
  | n < 10 = 1
  | otherwise = 1 + numDigits (n `quot` 10)
