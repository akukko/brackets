module Matchup (
    Matchup (..),
    sameTeam) where

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