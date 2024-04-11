module Histogram where

import Control.Arrow ((&&&))
import Data.List (sort)
import Text.Printf (printf)

import qualified Data.List.NonEmpty as NE

histogram ::  (Eq a, Ord a, Show a) => Int -> [a] -> [String]
histogram cols = histogram' cols . count

histogram' ::  (Show a, Show b, Real b) => Int -> [(a, b)] -> [String]
histogram' cols prs = map format $ zip3 items freqs bars
  where
    (items, freqs) = unzip prs
    format (i, f, b) = printf "%*s: %*s %s"
        (width items) (show i) (width freqs) (show f) b :: String
    bars = map (bar . normalize) freqs
    bar = flip replicate '*'
    normalize = round . (*scale) . realToFrac
    scale = realToFrac (cols - margin) / realToFrac (maximum freqs) :: Double
    margin = length $ format (0::Int, 0::Int, "")

width :: Show a => [a] -> Int
width = maximum . map (length . show)

count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = map (NE.head &&& length) . NE.group . sort
