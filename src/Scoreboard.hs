module Scoreboard (
    buildScoreboards,
    prettyScore,
    printableScoreboards) where

import Result
import Format

import Data.List
import Data.Maybe
import Numeric


data TeamScore = TeamScore {
    team :: String,
    games :: Int,
    points :: Int,
    totalScore :: Int,
    averageScore :: Double,
    bestGame :: Int
} deriving (Show, Eq)

instance Ord TeamScore where
    compare (TeamScore _ _ p1 _ a1 bg1) (TeamScore _ _ p2 _ a2 bg2) =
        if pd /= EQ then pd else (if ad /= EQ then ad else bgd) 
        where
            pd = compare p2 p1
            ad = compare a2 a1
            bgd = compare bg2 bg1

buildScoreboards :: [[Result]] -> [[TeamScore]]
buildScoreboards = map (\ r -> sort (buildScoreFromResult r []))

buildScoreFromResult :: [Result] -> [TeamScore] -> [TeamScore]
buildScoreFromResult (r:rs) ts
    | homeScore r > awayScore r = buildScoreFromResult rs (h 2:a 0:ts')
    | homeScore r < awayScore r = buildScoreFromResult rs (h 0:a 2:ts')
    | otherwise = buildScoreFromResult rs (h 1:a 1:ts')
    where
        h = buildScoreForTeam (home r) (homeScore r) oh
        a = buildScoreForTeam (away r) (awayScore r) oa
        oh = find (\x -> team x == home r) ts
        oa = find (\x -> team x == away r) ts
        ts' = if isJust oa then delete (fromJust oa) tsh else tsh
        tsh = if isJust oh then delete (fromJust oh) ts else ts
buildScoreFromResult [] ts = ts


buildScoreForTeam :: String -> Maybe Int -> Maybe TeamScore -> Int -> TeamScore
buildScoreForTeam team (Just s) (Just old) p =
    TeamScore team g' p' ts' as' bg'
    where 
        g' = games old + 1
        p' = points old + p
        ts' = totalScore old + s
        as' = (fromIntegral (totalScore old + s) / fromIntegral (games old + 1))
        bg' = if (bestGame old) < s then s else (bestGame old)

buildScoreForTeam team (Just s) Nothing p = TeamScore team 1 p s (fromIntegral s) s
buildScoreForTeam _ Nothing (Just old) _ = old
buildScoreForTeam team Nothing Nothing _ = TeamScore team 0 0 0 0 0
     



printableScoreboards :: Int -> [[TeamScore]] -> [String]
printableScoreboards mt = map (intercalate "\n" . addTitles mt . map (prettyScore mt))

addTitles :: Int -> [String] -> [String]
addTitles mt ss = eqLine:title:eqLine:ss++[dashLine]
    where
        title = intercalate sep (get mt titles)
        eqLine = concat (replicate (length title) "=")
        dashLine = concat (replicate (length title) "-")

prettyScore :: Int -> TeamScore -> String
prettyScore pad x = intercalate sep [t pad x, g x, p x, a x, tot x]


get mt (t:ts) = padRight mt ' ' t:ts

titles = ["Team", "Games", "Points", "Average", "Best"]

sep = " | "

gp = length (titles!!1)
pp = length (titles!!2)
as = length (titles!!3)
bg = length (titles!!4)

t pad x = padRight pad ' ' (team x)
g x = padLeft gp ' ' (show (games x))
p x = padLeft pp ' ' (show (points x))
a x = padLeft as ' ' (showFFloat (Just 2) (averageScore x) "")
tot x = padLeft bg ' ' (show (bestGame x))