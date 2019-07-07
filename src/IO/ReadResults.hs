module IO.ReadResults (
    readResults) where


import Result
import Matchup
import Data.Char

readResults :: String -> [[Result]]
readResults s = splitOnEmptyLine (lines s) [] []

splitOnEmptyLine :: [String] -> [Result] -> [[Result]] -> [[Result]]
splitOnEmptyLine (s:ss) c r
    | not (null s) = splitOnEmptyLine ss (c++[parse s]) r
    | otherwise = splitOnEmptyLine ss [] (r++[c])
splitOnEmptyLine [] c r = r++[c]

parse s = parseResult $ wordsWhen (==',') s

parseResult :: [String] -> Result
parseResult s = Result (sanitizedMatchup (s!!2) (s!!3)) (ri $ s!!0) (ri $ s!!1)

ri s
    | not (null (dropWhile isSpace s)) = Just (read s :: Int)
    | otherwise = Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = wordsWhenRec p s "" []

wordsWhenRec :: (Char -> Bool) -> String -> String -> [String] -> [String]
wordsWhenRec p (s:ss) r t
    | p s = wordsWhenRec p ss "" (t++[r])
    | otherwise = wordsWhenRec p ss (r++[s]) t
wordsWhenRec p [] r t = t++[r]