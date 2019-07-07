module Brackets (
    getPlayoffTeams,
    getFirstRound,
    getBrackets,
    updateBrackets,
    printableBrackets) where


import Scoreboard
import Matchup
import Result

import Data.List
import Data.Maybe

import Debug.Trace

-- i is the amount of progressing teams per group.

getPlayoffTeams :: Int -> [[TeamScore]] -> [TeamScore]
getPlayoffTeams i ts = sortBy betterAverage (concatMap (take i) ts)

betterAverage ts1 ts2 = compare (averageScore ts2) (averageScore ts1)

getFirstRound :: Int -> [[TeamScore]] -> [Result]
getFirstRound i ts = buildBrackets $ map team $ getPlayoffTeams i ts

buildBrackets :: [String] -> [Result]
buildBrackets ts = zipWith fromNames top bot'
    where
        (top, bot) = splitHalf ts
        bot' = reverse bot

getBrackets :: [Result] -> [[Result]]
getBrackets rs
    | null winners = []
    | otherwise = rs : getBrackets (buildBrackets winners)
    where winners = map (fromMaybe "TBA" . winner) rs

updateBrackets :: [[Result]] -> [[Result]]
updateBrackets rs = update rs []

update :: [[Result]] -> [[Result]] -> [[Result]]
update (r:rs) t
    | null winners && null t = getBrackets r
    | null winners = t ++ tail (getBrackets (last t))
    | otherwise = update rs (t++[r])
    where winners = mapMaybe winner r
update [] t
    | null t = []
    | otherwise = t ++ tail (getBrackets (last t))

printableBrackets :: Int -> [Result] ->  String
printableBrackets longest rs = intercalate "\n" $ map (prettyResult longest) rs


splitHalf l = splitAt ((length l + 1) `div` 2) l