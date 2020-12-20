module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram phrase = all (`elem` map toLower phrase) ['a' .. 'z']
