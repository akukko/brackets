module Result (
    Result (..)) where

import Matchup


data Result = Result {
    match :: Matchup,
    homeResult :: Maybe Int,
    awayResult :: Maybe Int
} deriving (Show)