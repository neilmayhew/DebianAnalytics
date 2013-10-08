module Main where

import Parse

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M

import Data.Functor
import Data.Maybe
import Data.List
import Text.Printf
import Control.Monad
import System.Environment

parseLines :: [L.ByteString] -> [LogLine]
parseLines = mapMaybe (AL.maybeResult . AL.parse line)

main :: IO ()
main = do
  [cmd, path] <- getArgs
  dispatch cmd path

type Command = String
type Action = [LogLine] -> IO ()

-- Looks up command in the list of actions, calls corresponding action.
dispatch :: Command -> FilePath -> IO ()
dispatch cmd path =
    action =<< parseLines . L.lines <$> L.readFile path
  where
    action = fromMaybe err (lookup cmd actions)
    err    = \_ -> putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

-- Associative list of commands and actions.
actions :: [(Command, Action)]
actions = [
    ("ips", topList . countIPs)
    ]

-- Helper that turns a map into a top list, based on the second value.
topList :: [(S.ByteString, Int)] -> IO ()
topList =
    let mostPopular (_,a) (_,b) = compare b a
    in mapM_ putStrLn . zipWith pretty [1..] . take 20 . sortBy mostPopular

-- Calculate a list of IPs and their counts
countIPs :: [LogLine] -> [(S.ByteString, Int)]
countIPs = M.toList . foldl' count M.empty
  where count acc x = M.insertWith (+) (S.copy $ llIP x) 1 acc

-- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n
