module Balance (
    balanceMatches ) where

import Matchup

balanceMatches = map (\single -> balance (single) [])

balance :: [Matchup] -> [Matchup] -> [Matchup]
balance (m:ms) new
    | isBalanced (m:ms) = balance (ms) (new++[m])
    | otherwise = balance (ms++[m]) (new)
balance [] new = new

isBalanced :: [Matchup] -> Bool
isBalanced [] = True
isBalanced ms 
    | sameTeamOccurs ms 3 = False
    | otherwise = isBalanced (tail ms)

sameTeamOccurs :: [Matchup] -> Int -> Bool
sameTeamOccurs ms i = same (take i ms) i [] 

same :: [Matchup] -> Int -> [String] -> Bool
same [] i s = numIsMore s i s
same (m:ms) i s = same ms i (home m:away m:s)

numIsMore :: [String] -> Int -> [String] -> Bool
numIsMore (m:ms) i orig
    | numTimesFound m orig >= i = True
    | otherwise = numIsMore ms i orig
numIsMore [] i orig = False

numTimesFound x = length . filter (== x)