module Parse where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as S

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

line :: Parser LogLine
line = do
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
