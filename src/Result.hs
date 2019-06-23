module Result (
    Result (..),
    prettyResult) where

import Matchup

import Data.Maybe


data Result = Result {
    match :: Matchup,
    homeScore :: Maybe Int,
    awayScore :: Maybe Int
} deriving (Show)

home :: Result -> String
home r = Matchup.home (match r)


away :: Result -> String
away r = Matchup.away (match r)

prettyResult :: Int -> Result -> String
prettyResult pad r = 
    padHome pad ' ' r ++ "  " ++ hs ++ " - " ++ as ++ "  " ++ padAway pad ' ' r 
    where 
        hs = case homeScore r of
            Just score -> padLeft scorePadAmount ' ' (show score)
            Nothing -> padLeft scorePadAmount ' ' ""
        as = case awayScore r of
            Just score -> padLeft scorePadAmount ' ' (show score)
            Nothing -> padLeft scorePadAmount ' ' ""

padHome pad char result = padLeft pad char (Result.home result)  
padAway pad char result = padRight pad char (Result.away result)


padLeft :: Int -> Char -> String -> String
padLeft i c s
    | length s >= i = s
    | otherwise = padLeft i c (c:s)

padRight :: Int -> Char -> String -> String
padRight i c s
    | length s >= i = s
    | otherwise = padRight i c (s++[c])

scorePadAmount = 4