module Result (
    Result (..),
    prettyResult,
    Result.away,
    Result.home,
    winner,
    fromMatchup,
    fromNames,
    sameMatches) where

import Matchup
import Format

import Data.Maybe


data Result = Result {
    match :: Matchup,
    homeScore :: Maybe Int,
    awayScore :: Maybe Int
} deriving (Show)

fromNames t b = Result (Matchup t b) Nothing Nothing
fromMatchup m = Result m Nothing Nothing

home :: Result -> String
home r = Matchup.home (match r)

away :: Result -> String
away r = Matchup.away (match r)

winner :: Result -> Maybe String
winner r
    | valid = findWinner r
    | otherwise = Nothing
    where 
        valid = isJust (homeScore r) && isJust (awayScore r)

-- This function assumes that both the results are not Nothing
findWinner :: Result -> Maybe String 
findWinner r
    | dif == GT = Just (Result.home r)
    | dif == LT = Just (Result.away r)
    | otherwise = Nothing
    where 
        dif = compare (fromJust $ homeScore r) (fromJust $ awayScore r)


prettyResult :: Int -> Result -> String
prettyResult pad r = 
    padHome pad ' ' r ++ "  " ++ hs ++ "  - " ++ as ++ "  " ++ padAway pad ' ' r 
    where 
        hs = case homeScore r of
            Just score -> padLeft scorePadAmount ' ' (show score)
            Nothing -> padLeft scorePadAmount ' ' ""
        as = case awayScore r of
            Just score -> padLeft scorePadAmount ' ' (show score)
            Nothing -> padLeft scorePadAmount ' ' ""

padHome pad char result = padLeft pad char (Result.home result)  
padAway pad char result = padRight pad char (Result.away result)


scorePadAmount = 4


sameMatches :: [Result] -> [Result] -> Bool
sameMatches (x:xs) (y:ys)
    | sameMatch xm ym = sameMatches xs ys 
    | otherwise = False
    where 
        xm = match x
        ym = match y
sameMatches [] [] = True
sameMatches [] _ = False
sameMatches _ [] = False