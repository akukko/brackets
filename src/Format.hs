module Format (
    padLeft,
    padRight,
    groupedInfo,
    combine) where

import Data.List


padLeft :: Int -> Char -> String -> String
padLeft i c s
    | length s >= i = s
    | otherwise = padLeft i c (c:s)

padRight :: Int -> Char -> String -> String
padRight i c s
    | length s >= i = s
    | otherwise = padRight i c (s++[c])

combine endSep = zipWith (\a b -> a ++ b ++ endSep)

groupedInfo xss cs fun = zipWith (f fun) xss cs
f fun xs c = c ++ intercalate "\n" (map fun xs)





