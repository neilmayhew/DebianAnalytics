-- Summarize apache log files by uri stem
--
-- Neil Mayhew - 2017-01-25

module Main where

import Parse

import Control.Arrow ((&&&), (***), second)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (comparing)
import Network.HTTP (rqURI)
import Network.URI (pathSegments)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

main :: IO ()
main = do
    [pathIn] <- getArgs
    entries <- map mkEntry <$> parseFile pathIn
    mapM_ (putStrLn . uncurry (printf "%s\t%s") . (show *** concat . NE.intersperse " " . NE.map show))
        . groupAssociations
        . uniq
        . map (leStem &&& leIP)
        . filter leSuccessful
        $ entries

leSuccessful :: LogEntry -> Bool
leSuccessful = (<400) . leStatus

leStem :: LogEntry -> Maybe [String]
leStem = either (const Nothing) (Just . take 2 . pathSegments . rqURI) . leReq

multiMapFromList :: Ord a => [(a, b)] -> M.Map a [b]
multiMapFromList = M.fromListWith (++) . map (second pure)

groupAssociations :: (Eq a, Ord a) => [(a, b)] -> [(a, NonEmpty b)]
groupAssociations = map ((fst . NE.head) &&& fmap snd)
    . NE.groupBy ((==) `on` fst) . sortBy (comparing fst)

associateOn :: (Eq a, Ord a) => (b -> a) -> [b] -> [(a, NonEmpty b)]
associateOn f = groupAssociations . map (f &&& id)

uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map NE.head . NE.group . sort
