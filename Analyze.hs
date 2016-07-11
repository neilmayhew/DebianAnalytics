-- Perform one of several analyses of apache log files
--
-- Neil Mayhew - 2013-10-08
--
-- Adapted from: https://variadic.me/posts/2012-02-25-adventures-in-parsec-attoparsec.html

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse

import Text.Blaze.Html4.Strict hiding (map, span, head)
import Text.Blaze.Html4.Strict.Attributes hiding (span)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

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
import Data.Ord (comparing)
import Data.Tuple
import Text.Printf
import Network.HTTP
import Network.URI
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad
import System.Environment
import System.FilePath

parseFile :: FilePath -> IO [LogLine]
parseFile path = parseLines . L.lines <$> readPath
  where readPath = if path == "-" then L.getContents else L.readFile path

parseLines :: [L.ByteString] -> [LogLine]
parseLines = mapMaybe (AL.maybeResult . AL.parse lineParser)

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
    ("ips",    topList . countItems . map (S.copy . llIP)),
    ("urls",   topList . countItems . filter notSvn . rights . map llPath),
    ("debs",   putDebs       . debCounts),
    ("users",  putArchUsers  . archUsers),
    ("arches", putArchCounts . archCounts),
    ("bad",    badReqs)]

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

groupFirsts :: (Ord a, Ord b) => [(a, b)] -> [(a, [b])]
groupFirsts = map combine . groupBy ((==) `on` fst) . sort
  where combine = fst . head &&& map snd

-- Calculate a list of items and their counts
countItems :: (Eq a, Hashable a) => [a] -> [(a, Int)]
countItems = M.toList . foldl' count M.empty
  where count acc x = M.insertWith (+) x 1 acc

type Arch = String
type IP = String

data Deb = Deb
    { debName    :: String
    , debVersion :: String
    , debArch    :: Arch
    } deriving (Eq, Ord, Show)

parseDeb :: FilePath -> Deb
parseDeb = toDeb . split '_' . dropExtension . takeFileName
  where toDeb [n, v, a] = Deb n v a

-- Count deb downloads
debCounts :: [LogLine] -> [(Int, [String])]
debCounts = groupFirsts . map swap . countItems . map debName . debs
  where
    debs :: [LogLine] -> [Deb]
    debs = map parseDeb . filter isDeb . rights . map llPath . filter isDownload

-- Count architectures
archCounts :: [LogLine] -> [(Arch, Int)]
archCounts = map (second length) . archUsers

-- Unique users of architectures
archUsers :: [LogLine] -> [(String, [IP])]
archUsers = groupFirsts . nub . arches . indices
  where
    arches :: [(IP, FilePath)] -> [(Arch, IP)]
    arches = map (swap . second arch)
    arch :: FilePath -> Arch
    arch = fromMaybe "?" . stripPrefix "binary-" . takeFileName . takeDirectory
    indices :: [LogLine] -> [(IP, FilePath)]
    indices = filter (isIndex . snd) . rights . map ipAndPath . filter isDownload
    ipAndPath l = (,) (S.unpack $ llIP l) <$> llPath l

-- Log line filtering predicates
isDownload = (=='2') . S.head . llStatus
notSvn = not . ("/svn/" `isPrefixOf`)
isDeb = (==".deb") . takeExtension
isIndex = (=="Packages") . takeBaseName

-- List package downloads
putDebs :: ToMarkup a => [(Int, [a])] -> IO ()
putDebs groups = putStr . renderHtml . docTypeHtml $ do
    H.head $ do
        H.title "DebianAnalytics"
        H.style ! A.type_ "text/css" $ do
            "table { border-collapse: collapse }"
            "td, th { border: 1px solid; padding: 0.25em; vertical-align: top }"
            ".count { text-align: right }"
            ".packages { text-align: left }"
    body $ do
        table $ do
            tr $ do
                th ! class_ "count" $ "Downloads"
                th ! class_ "packages" $ "Packages"
            forM_ (reverse groups) $ \(n, ps) -> do
                tr $ do
                    td ! class_ "count" $ do
                        toMarkup $ show n
                    td ! class_ "packages" $ do
                        sequence_ . intersperse br . map toMarkup $ ps

-- List architecture users
putArchUsers :: [(String, [IP])] -> IO ()
putArchUsers arches = do
    let width = maximum . map (length . fst) $ arches
    forM_ arches $ \(a, ips) ->
        putStrLn $ printf "%*s %s" width a (intercalate "," ips)

-- List architectures
putArchCounts :: [(String, Int)] -> IO ()
putArchCounts arches = do
    let width = length . show . maximum . map snd $ arches
    forM_ arches $ \(a, n) ->
        putStrLn $ printf "%*d %s" width n a

-- Show just the bad requests
badReqs :: [LogLine] -> IO ()
badReqs = mapM_ putStrLn . lefts . map llRequest

-- Extract the request from a log line
llRequest :: LogLine -> Either String Request_String
llRequest = AS.parseOnly requestParser . llReq

-- Extract the path of a request
llPath :: LogLine -> Either String String
llPath = return . uriPath . rqURI <=< llRequest

-- Split a string on a character
split :: Eq a => a -> [a] -> [[a]]
split c [] = []
split c s  = front : split c rest
  where (front, rest') = span (/= c) s
        rest = drop 1 rest'
