module Matchup (Matchup (..)) where

data Matchup = Matchup {
    home :: String,
    away :: String
} deriving (Show)