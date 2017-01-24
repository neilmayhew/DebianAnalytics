-- Perform one of several analyses of apache log files
--
-- Neil Mayhew - 2013-10-08
--
-- Adapted from: https://variadic.me/posts/2012-02-25-adventures-in-parsec-attoparsec.html

{-# LANGUAGE OverloadedStrings, DeriveGeneric, CPP #-}

module Main where

import Parse
import Color

import Text.Blaze.Html4.Strict (docTypeHtml, toMarkup, (!))
import Text.Blaze.Html.Renderer.Pretty

import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
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
import Data.String (fromString)
import Data.Time (UTCTime(..), parseTimeOrError, defaultTimeLocale, diffUTCTime)
import Data.Tuple
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import GHC.Generics (Generic)
import Text.Printf
import Network.HTTP
import Network.URI
import Control.Arrow (first, second, (&&&), (***))
import Control.Monad
import System.Environment
import System.FilePath

#if !MIN_VERSION_debian(3,89,0)
parseDebianVersion' = parseDebianVersion
#else
import Debian.Version (parseDebianVersion')
#endif

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
    err _  = putStrLn $ "Error: " ++ cmd ++ " is not a valid command."

-- Associative list of commands and actions.
actions :: [(Command, Action)]
actions = [
    ("ips",    topList . countItems . map (S.copy . llIP)),
    ("urls",   topList . countItems . filter notSvn . rights . map llPath),
    ("debs",   putDebs),
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
    , debVersion :: DebianVersion
    , debArch    :: Arch
    } deriving (Eq, Ord, Show, Generic)

instance Hashable DebianVersion where
    hashWithSalt n = hashWithSalt n . show . prettyDebianVersion

instance Hashable Deb

parseDeb :: FilePath -> Maybe Deb
parseDeb = toDeb . split '_' . dropExtension . unEscapeString . takeFileName
  where toDeb [n, v, a] = Just $ Deb n (parseDebianVersion' v) a
        toDeb _         = Nothing

-- Count deb downloads
extractDebs :: [LogLine] -> [Deb]
extractDebs = mapMaybe parseDeb . filter isDeb . rights . map llPath . filter isDownload

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
putDebs :: [LogLine] -> IO ()
putDebs entries = putStr . renderHtml . docTypeHtml $ do
    let (start, end) = (head &&& last) . map llTime $ entries
        timespan = show (utctDay start) ++ " – " ++ show (utctDay end)
        debs = extractDebs entries
        arches = sort . countItems . map debArch $ debs
        groups = groupFirsts . map swap . countItems . map debName $ debs
        pkgs = groupFirsts . map (debName . fst &&& id) . countItems $ debs
    H.head $ do
        H.title $ toMarkup $ "DebianAnalytics: " ++ timespan
        H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
        H.style ! A.type_ "text/css" $ mapM_ toMarkup
            [ "body {"
            , "    font-family: \"Lucida Grande\", Verdana, Tahoma, sans-serif;"
            , "    font-size: 90%;"
            , "    background-color: "++hsv(210,15,60)++";"
            , "    color: "++hsv(210,25,45)++";"
            , "    margin: 0;"
            , "}"
            , ":link, :visited {"
            , "    color: "++hsv(210,80,45)++";"
            , "}"
            , "table {"
            , "    border-collapse: collapse;"
            , "    margin: auto;"
            , "}"
            , "td, th {"
            , "    border: 1pt solid "++hsv(210,25,45)++";"
            , "    padding: 0.25em;"
            , "    vertical-align: top;"
            , "}"
            , "th {"
            , "    background-color: "++hsv(210,25,45)++";"
            , "    color: "++hsv(60,50,100)++";"
            , "    font-weight: normal;"
            , "    font-size: 115%;"
            , "}"
            , "td {"
            , "    background-color: "++hsv(210,8,100)++";"
            , "}"
            , ".title th {"
            , "    font-size: 175%;"
            , " }"
            , ".title td {"
            , "    text-align: center;"
            , " }"
            , ".arches td.count {"
            , "    width: 6.5em;"
            , "}"
            , ".packages td.count {"
            , "    width: 4em;"
            , "}"
            , ".count  { text-align: right; }"
            , ".name   { text-align: left; }"
            , ".filler { border: none; background-color: inherit; }"
            ]
    H.body $ do
        let total = sum $ map snd arches
        H.table ! A.class_ "title" $ do
            H.tr $ do
                H.th ! A.class_ "filler" $ "\xa0"
            H.tr $ do
                H.th ! A.class_ "name" $ toMarkup timespan
            H.tr $ do
                H.td ! A.class_ "name" $ fromString . printf "%.1f downloads/day" $
                    (fromIntegral total / realToFrac (end `diffUTCTime` start) * (60*60*24) :: Double)
        H.table ! A.class_ "arches" $ do
            H.tr $ do
                H.th ! A.class_ "filler" $ "\xa0"
            H.tr $ do
                H.th ! A.class_ "name"  $ "Arch"
                H.th ! A.class_ "count" $ "Downloads"
                H.th ! A.class_ "count" $ "Proportion"
            forM_ arches $ \(a, n) -> do
                H.tr $ do
                    H.td ! A.class_ "name" $ do
                        toMarkup a
                    H.td ! A.class_ "count" $ do
                        toMarkup n
                    H.td ! A.class_ "count" $ do
                        fromString $ printf "%.0f%%" (fromIntegral n / fromIntegral total * 100 :: Double)
            H.tr $ do
                H.td ! A.class_ "name" $ do
                    "Total"
                H.td ! A.class_ "count" $ do
                    toMarkup total
                H.td ! A.class_ "count" $ do
                    "100%"
        H.table ! A.class_ "groups" $ do
            H.tr $ do
                H.th ! A.class_ "filler" $ "\xa0"
            H.tr $ do
                H.th ! A.class_ "count" $ "Downloads"
                H.th ! A.class_ "name" $ "Packages"
            forM_ (reverse groups) $ \(n, ps) -> do
                H.tr $ do
                    H.td ! A.class_ "count" $ do
                        toMarkup n
                    H.td ! A.class_ "name" $ do
                        let pkgref p = H.a ! A.href (fromString $ '#':p) $ toMarkup p
                        sequence_ . intersperse H.br . map pkgref $ ps
        H.table ! A.class_ "packages" $ do
            forM_ pkgs $ \(name, dcs) -> do
                let arches = sort . nub . map (debArch . fst) $ dcs
                    versions = groupBy ((==) `on` debVersion . fst) dcs
                H.tr $ do
                    H.th ! A.class_ "filler" ! A.id (fromString name) $ "\xa0"
                H.tr $ do
                    H.th ! A.class_ "name" $ toMarkup name
                    forM_ arches $ \a -> do
                        H.th ! A.class_ "count" $ toMarkup a
                forM_ versions $ \dcs -> do
                    let archdebs = map (first debArch) dcs
                    H.tr $ do
                        H.td ! A.class_ "name" $ do
                            toMarkup . show . prettyDebianVersion . debVersion . fst . head $ dcs
                        forM_ arches $ \a -> do
                            H.td ! A.class_ "count" $ do
                                toMarkup . fromMaybe 0 $ lookup a archdebs
            H.tr $ do
                H.th ! A.class_ "filler" $ "\xa0"

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

-- Extract the request path of a log line
llPath :: LogLine -> Either String String
llPath = return . uriPath . rqURI <=< llRequest

-- Extract the timestamp of a log line
llTime :: LogLine -> UTCTime
llTime = parseTimeOrError False defaultTimeLocale "%d/%b/%Y:%T %z" . S.unpack . llDate

-- Split a string on a character
split :: Eq a => a -> [a] -> [[a]]
split c [] = []
split c s  = front : split c rest
  where (front, rest') = span (/= c) s
        rest = drop 1 rest'
