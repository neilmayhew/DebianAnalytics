-- Parse apache log files
--
-- Neil Mayhew - 2013-10-08
--
-- Adapted from: https://variadic.me/posts/2012-02-25-adventures-in-parsec-attoparsec.html

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}

module Parse
    ( LogEntry(..)
    , LogLine(..)
    , mkEntry
    , renderLine
    , lineParser
    , llRequest
    , llPath
    , llTime
    , parseFile
    , parseLines
    , renderFile
    ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Network.HTTP
import Network.URI
import Data.IP
import Data.Time (UTCTime(..), parseTimeOrError, defaultTimeLocale)

import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import Control.Monad ((<=<), void)

import Control.DeepSeq -- (NFData(..))
import GHC.Generics (Generic)

data LogEntry = LogEntry
    { leIP     :: IP
    , leIdent  :: Maybe String
    , leUser   :: Maybe String
    , leDate   :: UTCTime
    , leReq    :: Either String Request_String
    , leStatus :: Int
    , leBytes  :: Int
    , leRef    :: Maybe URI
    , leUA     :: String
    } deriving (Show, Eq, Ord, Generic, NFData)

mkEntry :: LogLine -> LogEntry
mkEntry l = LogEntry
    (read . S.unpack $ llIP l)
    (maybeString $ llIdent l)
    (maybeString $ llUser l)
    (llTime l)
    (llRequest l)
    (read . S.unpack $ llStatus l)
    (read . S.unpack $ llBytes l)
    (parseURI . S.unpack $ llRef l)
    (S.unpack $ llUA l)
  where
    maybeString "-" = Nothing
    maybeString x = Just $ S.unpack x

data LogLine = LogLine
    { llIP     :: S.ByteString
    , llIdent  :: S.ByteString
    , llUser   :: S.ByteString
    , llDate   :: S.ByteString
    , llReq    :: S.ByteString
    , llStatus :: S.ByteString
    , llBytes  :: S.ByteString
    , llRef    :: S.ByteString
    , llUA     :: S.ByteString
    } deriving (Show, Eq, Ord, Generic, NFData)

instance NFData IPv4
instance NFData IPv6
instance NFData IP

deriving instance Generic RequestMethod
deriving instance Generic Header
deriving instance Generic a => Generic (Request a)
instance NFData RequestMethod where rnf x = seq x ()
instance NFData Header where rnf x = seq x ()
instance Generic a => NFData (Request a) where rnf x = seq x ()

-- Extract the request from a log line
llRequest :: LogLine -> Either String Request_String
llRequest = parseOnly requestParser . llReq

-- Extract the request path of a log line
llPath :: LogLine -> Either String String
llPath = return . uriPath . rqURI <=< llRequest

-- Extract the timestamp of a log line
llTime :: LogLine -> UTCTime
llTime = parseTimeOrError False defaultTimeLocale "%d/%b/%Y:%T %z" . S.unpack . llDate

renderLine :: LogLine -> S.ByteString
renderLine (LogLine ip id u d req s n ref ua) =
        S.intercalate (S.singleton ' ') [ip,id,u,b d,q req,s,n,q ref,q ua]
  where b v = S.concat [S.singleton '[', v, S.singleton ']']
        q v = S.concat [S.singleton '"', v, S.singleton '"']

quote, lbrack, rbrack :: Parser Char
quote  = satisfy (== '\"')
lbrack = satisfy (== '[')
rbrack = satisfy (== ']')

quote_, lbrack_, rbrack_, space_ :: Parser ()
quote_  = void quote
lbrack_ = void lbrack
rbrack_ = void rbrack
space_  = void space

plainValue :: Parser S.ByteString
plainValue = takeTill (== ' ')

quotedValue :: Parser S.ByteString
quotedValue = do
    quote_
    res <- takeTill (== '\"')
    quote_
    return res

bracketedValue :: Parser S.ByteString
bracketedValue = do
    lbrack_
    res <- takeTill (== ']')
    rbrack_
    return res

combined :: Parser (S.ByteString,S.ByteString)
combined = do
    space_
    path <- quotedValue
    space_
    ua <- quotedValue
    return (path,ua)

lineParser :: Parser LogLine
lineParser = do
    ip <- plainValue
    space_
    identity <- plainValue
    space_
    user <- plainValue
    space_
    date <- bracketedValue
    space_
    req <- quotedValue
    space_
    status <- plainValue
    space_
    bytes <- plainValue
    (path,ua) <- option (S.empty,S.empty) combined
    return $ LogLine ip identity user date req status bytes path ua

deriving instance Read RequestMethod
deriving instance Ord  RequestMethod
deriving instance Ord  HeaderName
deriving instance Ord  Header
deriving instance Eq   Header
deriving instance Eq   Request_String
deriving instance Ord  Request_String

requestParser :: Parser (HTTPRequest String)
requestParser = do
    method <- mkMethod . S.unpack <$> plainValue
    space_
    uri <- S.unpack <$> plainValue
    case parseURIReference uri of
        Nothing -> fail ("Invalid URI: " ++ uri)
        Just u -> return $ Request u method [] ""
  where
    mkMethod s = case reads s of
        (method,_):_ -> method
        _            -> Custom s

parseFile :: FilePath -> IO [LogLine]
parseFile path = parseLines . L.lines <$> readPath
  where readPath = if path == "-" then L.getContents else L.readFile path

parseLines :: [L.ByteString] -> [LogLine]
parseLines = mapMaybe (AL.maybeResult . AL.parse lineParser)

renderFile :: FilePath -> [LogLine] -> IO ()
renderFile path = writePath . S.unlines . map renderLine
  where writePath = if path == "-" then S.putStr else S.writeFile path
