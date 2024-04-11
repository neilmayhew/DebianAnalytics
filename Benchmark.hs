{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

-- Benchmark parsing of Duplicity backups

import Criterion.Main
import Control.DeepSeq
import Control.Monad ((<=<))
import Control.Exception (evaluate)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.Char8 as L

import Parse

main :: IO ()
main = do
    logs <- evaluate . force . L.lines =<< L.getContents

    defaultMain
        [ bench "parseLines"
            $ nf parseLines logs
        , bench "mkEntry"
            $ nf (map mkEntry . parseLines) logs
        ]

readLogs :: [FilePath] -> IO [L.ByteString]
readLogs paths = concat <$> mapM readLines paths
  where readLines = return . L.lines <=< readPath
        readPath path = if path == "-" then L.getContents else L.readFile path
