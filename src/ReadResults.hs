module ReadResults (
    readResults) where


import Result
import Matchup
import Data.Char
import Debug.Trace

readResults :: String -> [[Result]]
readResults s = splitOnEmptyLine (lines s) [] []

splitOnEmptyLine :: [String] -> [Result] -> [[Result]] -> [[Result]]
splitOnEmptyLine (s:ss) c r
    | not (null s) = splitOnEmptyLine ss (c++[parse s]) r
    | otherwise = splitOnEmptyLine ss [] (r++[c])
splitOnEmptyLine [] c r = r++[c]

parse s = parseResult $ wordsWhen (==',') s

parseResult :: [String] -> Result
parseResult s = Result (matchup (s!!0) (s!!1)) (ri $ s!!2) (ri $ s!!3)

ri s 
    | not (null (dropWhile isSpace s)) = Just (read s :: Int)
    | otherwise = Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'