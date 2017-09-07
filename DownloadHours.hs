-- Summarize CKAN downloads per hour
--
-- Neil Mayhew - 2017-09-01

module Main where

import Parse
import Histogram

import Data.Time (UTCTime(..), secondsToDiffTime)
import Network.HTTP (rqURI)
import Network.URI (pathSegments)
import System.Environment (getArgs, lookupEnv)

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

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&. g = \x -> f x && g x
