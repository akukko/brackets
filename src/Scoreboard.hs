module Scoreboard (
    TeamScore (..),
    buildScoreboards,
    prettyScore,
    printableScoreboards) where

import Result
import Format
import TeamScore

import Data.List



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
        ts' = case oa of
            Just oa' -> delete oa' tsh
            Nothing -> tsh
        tsh = case oh of
            Just oh' -> delete oh' ts
            Nothing -> ts
buildScoreFromResult [] ts = ts


buildScoreForTeam :: String -> Maybe Int -> Maybe TeamScore -> Int -> TeamScore
buildScoreForTeam team (Just s) (Just old) p =
    TeamScore team g' p' ts' as' bg'
    where
        g' = games old + 1
        p' = points old + p
        ts' = totalScore old + s
        as' = fromIntegral (totalScore old + s) / fromIntegral (games old + 1)
        bg' = if bestGame old < s then s else bestGame old

buildScoreForTeam team (Just s) Nothing p = TeamScore team 1 p s (fromIntegral s) s
buildScoreForTeam _ Nothing (Just old) _ = old
buildScoreForTeam team Nothing Nothing _ = TeamScore team 0 0 0 (-160) (-160)


printableScoreboards :: Int -> [[TeamScore]] -> [String]
printableScoreboards mt = 
    map (intercalate "\n" . addTitles mt . (map (uncurry (prettyScore mt)) . addPlacements))

addTitles :: Int -> [String] -> [String]
addTitles mt ss = eqLine:titles:eqLine:ss++[eqLine]
    where
        titles = getTitles mt
        eqLine = concat (replicate (length titles) "=")

addPlacements :: [TeamScore] -> [(Int, TeamScore)]
addPlacements = zip (map fromIntegral [1..])

