-- Filter apache log files by URI path
--
-- Neil Mayhew - 2013-12-04

module Main where

import Parse

import System.Path.WildMatch
import System.Environment

main :: IO ()
main = do
  [pattern, pathIn, pathOut] <- getArgs
  input <- parseFile pathIn
  let result = filter (pathPred $ wildCheckCase pattern) input
  renderFile pathOut result

pathPred :: (String -> Bool) -> (LogLine -> Bool)
pathPred p = either (const False) p . llPath
