-- Filter apache log files by request status
--
-- Neil Mayhew - 2013-12-16

module Main where

import Parse

import qualified Data.ByteString.Char8 as S

import System.Environment

main :: IO ()
main = do
    [pathIn, pathOut] <- getArgs
    input <- parseFile pathIn
    let result = filter desired input
    renderFile pathOut result
  where
    desired l = S.head (llStatus l) == '2'

