module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (genericLength)

encode :: String -> String
encode = unwords . mergeRows . splitInRect . map toLower . filter isAlphaNum

mergeRows :: [String] -> [String]
mergeRows rows = foldr (zipWith (:)) emptyList rows
  where
    width = length . (!! 0) $ rows
    emptyList = replicate width ""

splitInRect :: String -> [String]
splitInRect text =
  chunkBy size text
  where
    size = ceiling . sqrt . genericLength $ text

chunkBy :: Int -> String -> [String]
chunkBy size text
  | rest == "" = [padRight size ' ' chunk]
  | otherwise = chunk : chunkBy size rest
  where
    (chunk, rest) = splitAt size text

padRight :: Int -> a -> [a] -> [a]
padRight size x list = list ++ replicate (size - length list) x
