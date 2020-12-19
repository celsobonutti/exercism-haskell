module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram = (== 26) . length . nub . filter (\l -> isAscii l && isAlpha l) . map toLower
