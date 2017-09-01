-- Summarize CKAN downloads per hour
--
-- Neil Mayhew - 2017-09-01

module Main where

import Parse

import Control.Arrow ((&&&), (***), second)
import Data.List (group, sort)
import Data.Time (UTCTime(..), DiffTime(..), secondsToDiffTime, getCurrentTime)
import Network.HTTP (rqURI)
import Network.URI (pathSegments)
import System.Environment (getArgs, lookupEnv)
import Text.Printf (printf)

import qualified Data.Map as M

main :: IO ()
main = do
    paths <- getArgs
    cols <- maybe 80 read <$> lookupEnv "COLUMNS"
    entries <- map mkEntry . concat <$> mapM parseFile paths
    putStr . unlines . histogram cols . downloadHours $ entries

downloadHours :: [LogEntry] -> [Int]
downloadHours = map (hourOfDay . leDate) . filter (leIsDownload .&&. leIsSuccessful)

hourOfDay :: UTCTime -> Int
hourOfDay t = floor (utctDayTime t / secondsToDiffTime 3600)

leIsDownload :: LogEntry -> Bool
leIsDownload = either (const False) (("download" `elem`) . pathSegments . rqURI) . leReq

leIsSuccessful :: LogEntry -> Bool
leIsSuccessful = (==200) . leStatus

histogram ::  (Eq a, Ord a, Show a) => Int -> [a] -> [String]
histogram cols xs = map format $ zip3 items freqs bars
  where
    (items, freqs) = unzip $ count xs
    format (i, f, b) = printf "%*s: %*s %s"
        (width items) (show i) (width freqs) (show f) b :: String
    bars = map (bar . normalize) freqs
    bar = flip replicate '*'
    normalize = round . (*scale) . realToFrac
    scale = realToFrac (cols - margin) / realToFrac (maximum freqs)
    margin = length $ format (0, 0, "")

width :: Show a => [a] -> Int
width = maximum . map (length . show)

count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = map (head &&& length) . group . sort

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&. g = \x -> f x && g x
