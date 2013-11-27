module Main where

import Parse

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M

import Data.Either
import Data.Functor
import Data.Function
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Tuple
import Text.Printf
import Network.HTTP
import Network.URI
import Control.Monad
import System.Environment
import System.FilePath

parseFile :: FilePath -> IO [LogLine]
parseFile path = parseLines . L.lines <$> readPath
  where readPath = if path == "-" then L.getContents else L.readFile path

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
    action =<< parseFile path
  where
    action = fromMaybe err (lookup cmd actions)
    err    = \_ -> putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

-- Associative list of commands and actions.
actions :: [(Command, Action)]
actions = [
    ("ips",  topList . countFields . map (S.copy . llIP)),
    ("urls", topList . countFields . filter notSvn . rights . map llPath),
    ("debs", countDebs . filter isDeb . map takeFileName . rights . map llPath),
    ("bad",  badReqs)]

topList :: Show a => [(a, Int)] -> IO ()
topList = putList . take 20 . sortList

-- Helper that turns a map into a top list, based on the second value.
putList :: Show a => [(a, Int)] -> IO ()
putList = mapM_ putStrLn . zipWith pretty [1..]

sortList :: [(a, Int)] -> [(a, Int)]
sortList = sortBy (flip compare `on` snd)

-- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

groupCounts :: (Ord a, Hashable a) => [(a, Int)] -> [(Int, [a])]
groupCounts = map combine . groupBy ((==) `on` fst) . sort . map swap
  where combine g = (fst . head $ g, map snd g)

-- Calculate a list of field values and their counts
countFields :: (Eq a, Hashable a) => [a] -> [(a, Int)]
countFields = M.toList . foldl' count M.empty
  where count acc x = M.insertWith (+) x 1 acc

-- Log line filtering predicates
notSvn = not . ("/svn/" `isPrefixOf`)
isDeb = (== ".deb") . takeExtension

-- Count package downloads
countDebs :: [String] -> IO ()
countDebs debs = do
    putStrLn $ "<html><head><style type='text/css'>"
        ++ "table { border-collapse: collapse }"
        ++ "td, th { border: 1px solid; padding: 0.25em; vertical-align: top }"
        ++ ".count { text-align: right }"
        ++ ".packages { text-align: left }"
        ++ "</style><body><table>"
        ++ "<tr><th class='count'>Downloads</th><th class='packages'>Packages</th></tr>"
    putCounts . reverse . groupCounts . countFields . map (head . parseDeb) $ debs
    putStrLn "</table></body></html>"
  where
    parseDeb = split '_' . dropExtension
    putCounts = mapM_ $ putStrLn . showGroup
    showGroup (n, ps) = printf "<tr><td class='count'>%d</td><td class='packages'>%s</td></tr>" n (intercalate "<br/>" ps)

-- Show just the bad requests
badReqs :: [LogLine] -> IO ()
badReqs = mapM_ putStrLn . lefts . map llRequest

-- Extract the request from a log line
llRequest :: LogLine -> Either String Request_String
llRequest = AS.parseOnly request . llReq

-- Extract the path of a request
llPath :: LogLine -> Either String String
llPath = return . uriPath . rqURI <=< llRequest

-- Split a string on a character
split :: Eq a => a -> [a] -> [[a]]
split c [] = []
split c s  = front : split c rest
  where (front, rest') = span (/= c) s
        rest = drop 1 rest
