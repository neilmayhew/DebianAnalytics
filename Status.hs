-- Filter apache log files by request status
--
-- Neil Mayhew - 2013-12-16

module Main where

import Parse

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Functor
import Data.Maybe
import System.Environment

main :: IO ()
main = do
    [pathIn, pathOut] <- getArgs
    input <- parseFile pathIn
    let result = filter desired input
    renderFile pathOut result
  where
    desired l = S.head (llStatus l) == '4'

parseFile :: FilePath -> IO [LogLine]
parseFile path = parseLines . L.lines <$> readPath
  where readPath = if path == "-" then L.getContents else L.readFile path

parseLines :: [L.ByteString] -> [LogLine]
parseLines = mapMaybe (AL.maybeResult . AL.parse lineParser)

renderFile :: FilePath -> [LogLine] -> IO ()
renderFile path = writePath . S.unlines . map renderLine
  where writePath = if path == "-" then S.putStr else S.writeFile path

