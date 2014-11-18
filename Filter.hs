-- Filter apache log files by URI path
--
-- Neil Mayhew - 2013-12-04

module Main where

import Parse

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Network.HTTP
import Network.URI
import System.Path.WildMatch

import Data.Functor
import Data.List
import Data.Maybe
import Control.Monad
import System.Environment

main :: IO ()
main = do
  [pattern, pathIn, pathOut] <- getArgs
  input <- parseFile pathIn
  let result = filter (pathPred $ wildCheckCase pattern) input
  renderFile pathOut result

parseFile :: FilePath -> IO [LogLine]
parseFile path = parseLines . L.lines <$> readPath
  where readPath = if path == "-" then L.getContents else L.readFile path

parseLines :: [L.ByteString] -> [LogLine]
parseLines = mapMaybe (AL.maybeResult . AL.parse lineParser)

renderFile :: FilePath -> [LogLine] -> IO ()
renderFile path = writePath . S.unlines . map renderLine
  where writePath = if path == "-" then S.putStr else S.writeFile path

pathPred :: (String -> Bool) -> (LogLine -> Bool)
pathPred p = either (const False) p . llPath

-- Extract the path of a request
llPath :: LogLine -> Either String String
llPath = return . uriPath . rqURI <=< llRequest

-- Extract the request from a log line
llRequest :: LogLine -> Either String Request_String
llRequest = AS.parseOnly requestParser . llReq
