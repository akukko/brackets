module TeamScore (
    TeamScore (..),
    prettyScore,
    getTitles) where

import Format

import Data.List
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
    compare (TeamScore _ _ p1 _ a1 bg1) (TeamScore _ _ p2 _ a2 bg2)
      | pd /= EQ = pd
      | ad /= EQ = ad
      | otherwise = compare bg2 bg1
      where pd = compare p2 p1
            ad = compare a2 a1


prettyScore :: Int -> Int -> TeamScore -> String
prettyScore pad rank x = addSeparators [r 2 rank, t pad x, g x, p x, a x, tot x]

getTitles mt = addSeparators (padTeamName mt titles)

addSeparators s = "| "++intercalate sep s++" |"

-- mt is max team name lengnth
padTeamName mt (a:b:ts) = a:padRight mt ' ' b:ts


titles = ["  ", "Team", "Games", "Points", "Average", "Best"]

sep = " | "

gp = length (titles!!2)
pp = length (titles!!3)
as = length (titles!!4)
bg = length (titles!!5)

r pad x = padLeft pad ' ' (show x)
t pad x = padRight pad ' ' (team x)
g x = padLeft gp ' ' (show (games x))
p x = padLeft pp ' ' (show (points x))
a x = padLeft as ' ' (showFFloat (Just 2) (averageScore x) "")
tot x = padLeft bg ' ' (show (bestGame x))