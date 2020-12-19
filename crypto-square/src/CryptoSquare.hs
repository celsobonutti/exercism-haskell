module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (genericLength, transpose)

encode :: String -> String
encode = unwords . transpose . splitInRect . map toLower . filter isAlphaNum

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
