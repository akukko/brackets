module Balance (
    balanceMatches) where

import Matchup


balanceMatches limit = map (\x -> 
                    if length x < limit 
                    then balanceSparse x 
                    else balanceConcurrent x)

balanceConcurrent :: [Matchup] -> [Matchup]
balanceConcurrent ms = concurrent ms [] []

balanceSparse :: [Matchup] -> [Matchup]
balanceSparse ms = sparse ms [] []


{- 

Concurrent playing order. Order the games so that two games can be played
simultaneously. Used for larger groups to save time.

Based on empiric research, this algorithm doesn't seem to get stuck in an
infinite loop. This has not been mathematically proven.

-}

concurrent :: [Matchup] -> [Matchup] -> [Matchup] -> [Matchup]
concurrent (f:s:ms) (fb:sb:bs) r
    | not $ sameTeam fb sb = concurrent (f:s:ms) bs (r++fb:[sb])
    | not $ sameTeam f fb = concurrent ms (s:sb:bs) (r++f:[fb])
    | not $ sameTeam f sb = concurrent ms (s:fb:bs) (r++f:[sb])
    | not $ sameTeam s fb = concurrent ms (f:sb:bs) (r++s:[fb])
    | not $ sameTeam s sb = concurrent ms (f:fb:bs) (r++s:[sb])
    | not $ sameTeam f s = concurrent ms (fb:sb:bs) (r++f:[s])
    | otherwise = concurrent ms (s:f:fb:sb:bs) r
concurrent [] (fb:sb:bs) r
    | sameTeam fb sb = concurrent (reverse (r++bs++fb:[sb])) [] []
    | otherwise = concurrent [] bs (r++fb:[sb])
concurrent (f:s:ms) [] r
    | sameTeam f s = concurrent ms (f:[s]) r
    | otherwise = concurrent ms [] (r++f:[s])
concurrent (m:ms) (b:bs) r = concurrent ms (m:b:bs) r
concurrent [] [b] r = r++[b]
concurrent [m] [] r = r++[m]
concurrent [] [] r = r


{-

Sparse playing order. Don't make three consecutive matchups.

Build a new list of matchups, and if the same team appears three times in a
row, add that matchup to buffer. At the beginning of each iteration, try to
empty the buffer. If the result is not three games for the same team in a 
row, add the first matchup in the buffer to the end of the matchup list.

Based on empiric research, this algorithm doesn't seem to get stuck in an
infinite loop. This has not been mathematically proven.

-}

sparse :: [Matchup] -> [Matchup] -> [Matchup] -> [Matchup]
sparse (m:ms) (b:bs) r
    | not $ sameTeamOccurs (b:r) 3 = sparse (m:ms) bs (b:r)
    | not $ sameTeamOccurs (m:r) 3 = sparse ms (b:bs) (m:r)
    | otherwise = sparse ms (m:b:bs) r
sparse (m:ms) [] r
    | not $ sameTeamOccurs (m:r) 3 = sparse ms [] (m:r)
    | otherwise = sparse ms [m] r
sparse [] (b:bs) r
    | not $ sameTeamOccurs (b:r) 3 = sparse [] bs (b:r)
    | otherwise = sparse (reverse (bs++r++[b])) [] []
sparse [] [] r = r



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
