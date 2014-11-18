-- Parse apache log files
--
-- Neil Mayhew - 2013-10-08
--
-- Adapted from: https://variadic.me/posts/2012-02-25-adventures-in-parsec-attoparsec.html

{-# LANGUAGE StandaloneDeriving #-}

module Parse (LogLine(..), renderLine, lineParser, requestParser) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as S

import Network.HTTP
import Network.URI

import Data.Functor

data LogLine = LogLine {
    llIP     :: S.ByteString,
    llIdent  :: S.ByteString,
    llUser   :: S.ByteString,
    llDate   :: S.ByteString,
    llReq    :: S.ByteString,
    llStatus :: S.ByteString,
    llBytes  :: S.ByteString,
    llRef    :: S.ByteString,
    llUA     :: S.ByteString
} deriving (Ord, Show, Eq)

renderLine :: LogLine -> S.ByteString
renderLine (LogLine ip id u d req s n ref ua) =
        S.intercalate (S.singleton ' ') [ip,id,u,b d,q req,s,n,q ref,q ua]
  where b v = S.concat [S.singleton '[', v, S.singleton ']']
        q v = S.concat [S.singleton '"', v, S.singleton '"']

quote, lbrack, rbrack :: Parser Char
quote  = satisfy (== '\"')
lbrack = satisfy (== '[')
rbrack = satisfy (== ']')

plainValue :: Parser S.ByteString
plainValue = takeTill (== ' ')

quotedValue :: Parser S.ByteString
quotedValue = do
    quote
    res <- takeTill (== '\"')
    quote
    return res

bracketedValue :: Parser S.ByteString
bracketedValue = do
    lbrack
    res <- takeTill (== ']')
    rbrack
    return res

combined :: Parser (S.ByteString,S.ByteString)
combined = do
    space
    path <- quotedValue
    space
    ua <- quotedValue
    return (path,ua)

lineParser :: Parser LogLine
lineParser = do
    ip <- plainValue
    space
    identity <- plainValue
    space
    user <- plainValue
    space
    date <- bracketedValue
    space
    req <- quotedValue
    space
    status <- plainValue
    space
    bytes <- plainValue
    (path,ua) <- option (S.empty,S.empty) combined
    return $ LogLine ip identity user date req status bytes path ua

deriving instance Read RequestMethod

requestParser :: Parser (HTTPRequest String)
requestParser = do
    method <- mkMethod . S.unpack <$> plainValue
    space
    uri' <- plainValue
    case parseURIReference (S.unpack uri') of
        Nothing -> fail ("Invalid URI: " ++ S.unpack uri')
        Just uri -> return $ Request uri method [] ""
  where
    mkMethod s = case reads s of
        [] -> Custom s
        (method,_):_ -> method
