module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
    length filtered >= 2 && (rem nSum 10 == 0)     
    where
        filtered = filter isDigit n
        nSum = calculateSum filtered


calculateSum :: String -> Int
calculateSum xs = 
    sum . zipWith (curry doubleOdd) [0..length xs] . map digitToInt . reverse $ xs
    

doubleOdd :: (Int, Int) -> Int
doubleOdd (index, n) 
    | even index =
        n 
    | otherwise =
        if m > 9 then
            m - 9
        else
            m
    where
        m = n * 2
