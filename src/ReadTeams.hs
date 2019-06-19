module ReadTeams
( readTeams
) where

import System.IO
import Data.Char

readTeams :: [String] -> [String]
readTeams t = readTeamsRec t [] 

readTeamsRec :: [String] -> [String] -> [String]
readTeamsRec (x:xs) a
    -- | head x == '!' = readTeamsRec xs a
    | null (dropWhile isSpace x) = readTeamsRec xs a -- ignore empty lines
    | otherwise = readTeamsRec xs (x:a)
readTeamsRec [] a = a
