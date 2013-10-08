module Main where

import Parse

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M

import Data.Maybe
import Data.List
import Text.Printf
import Control.Monad
import System.Environment

countIPs :: [L.ByteString] -> [(S.ByteString,Int)]
countIPs = M.toList . foldl' count M.empty
    where
        count acc l = case AL.maybeResult $ AL.parse line l of
            Just x  -> M.insertWith (+) (S.copy $ llIP x) 1 acc
            Nothing -> acc

main :: IO ()
main = do
  [cmd, path] <- getArgs
  dispatch cmd path

type Command = String

-- Looks up command in the list of actions, calls corresponding action.
dispatch :: Command -> FilePath -> IO ()
dispatch cmd path = action path
    where
        action = fromMaybe err (lookup cmd actions)
        err    = \_ -> putStrLn $ "Error: " ++ cmd ++ " is not a valid command."
                    
-- Associative list of commands and actions.
actions :: [(Command, FilePath -> IO ())]
actions = [
    ("ips", mapToTopList countIPs)
	]

-- Helper that turns a map into a top list, based on the second value.
mapToTopList :: ([L.ByteString] -> [(S.ByteString, Int)]) -> FilePath -> IO ()
mapToTopList f p = do
    file <- liftM L.lines $ L.readFile p
    let mostPopular (_,a) (_,b) = compare b a
        m = f file
    mapM_ putStrLn . zipWith pretty [1..] . take 20 . sortBy mostPopular $ m

-- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n
