module IO.ReadTeams ( 
    readTeams) where

import System.IO
import Data.Char

readTeams :: [String] -> [String]
readTeams t = readTeamsRec t [] 

readTeamsRec :: [String] -> [String] -> [String]
readTeamsRec (x:xs) a
    | null (dropWhile isSpace x) = readTeamsRec xs a -- ignore empty lines
    | head x == '#' = readTeamsRec xs a
    | head x == '!' = a
    | otherwise = readTeamsRec xs (a++[x])
readTeamsRec [] a = a
