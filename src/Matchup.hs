module Matchup (
    Matchup (..),
    sameTeam,
    sameMatch) where

data Matchup = Matchup {
    home :: String,
    away :: String
} deriving (Show)


sameTeam :: Matchup -> Matchup -> Bool
sameTeam x y
    | away x == away y = True
    | away x == home y = True
    | home x == away y = True
    | home x == home y = True
    | otherwise = False

sameMatch :: Matchup -> Matchup -> Bool
sameMatch x y
    | away x == away y && home x == home y = True 
    | away x == home y && home x == away y = True
    | home x == away y && away x == home y = True
    | home x == home y && away x == away y = True
    | otherwise = False