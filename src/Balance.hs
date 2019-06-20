module Balance (
    balanceMatches ) where

import Matchup

balanceMatches = map balance

balance :: [Matchup] -> [Matchup]
balance ms = bal ms [] []

-- Kasataan uutta listaa matchupeista ja laitetaan kolmas sama joukkue
-- bufferiin jos sellaisia tulee. Bufferia yritetään tyhjentää joka iteraatiolla, 
-- jos se tarkoittaisi ettei tule kolme samaa putkeen.

-- Empiirisen tutkimuksen perusteella tämä ei jää ikuisesti jumiin,
-- mutta sitä ei ole todistettu.

bal :: [Matchup] -> [Matchup] -> [Matchup] -> [Matchup]
bal (m:ms) (b:bs) r
    | not $ sameTeamOccurs (b:r) 3 = bal (m:ms) bs (b:r) 
    | sameTeamOccurs (m:r) 3 = bal ms (b:bs) (m:r) 
    | otherwise = bal ms (m:b:bs) r
bal (m:ms) [] r 
    | not $ sameTeamOccurs (m:r) 3 = bal ms [] (m:r) 
    | otherwise = bal ms [m] r
bal [] (b:bs) r 
    | not $ sameTeamOccurs (b:r) 3 = bal [] bs (b:r)
    | otherwise = bal (reverse (bs++r++[b])) [] []
bal [] [] r = r

sameTeamOccurs :: [Matchup] -> Int -> Bool
sameTeamOccurs ms i = same (take i ms) i [] 

same :: [Matchup] -> Int -> [String] -> Bool
same (m:ms) i s = same ms i (home m:away m:s)
same [] i s = numIsMore s i s

numIsMore :: [String] -> Int -> [String] -> Bool
numIsMore (m:ms) i orig
    | numTimesFound m orig >= i = True
    | otherwise = numIsMore ms i orig
numIsMore [] i orig = False

numTimesFound x = length . filter (== x)
