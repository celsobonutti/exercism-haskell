module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram  
    where
        toLowerWord = map toLower
        toSortedLower = sort . toLowerWord
        lowered = toLowerWord word
        sorted = sort lowered
        isAnagram w = lowered /= toLowerWord w && sorted == toSortedLower w
