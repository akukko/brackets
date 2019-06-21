module Brackets (
    getGroups,
    getSchedule,
    test) where

import Matchup
import Balance
import Data.List
import Debug.Trace

-- Takes a list of teams, and the max number of teams per group.
-- Returns a list of groups.
getGroups :: [String] -> Int -> [[String]]
getGroups s max = case groupAmount of
    Just amt -> splitToGroups s amt 0 []
    Nothing -> []
    where
        groupAmount = calculateGroupAmount (length s) max

-- Takes a list of groups. 
-- Returns a list of group's matchups.
getSchedule :: [[String]] -> Int -> [[Matchup]]
getSchedule g limit = reverse $ balanceMatches limit $ recursive g []

groupAmounts = iterate (*2) 1

calculateGroupAmount :: Int -> Int -> Maybe Int
calculateGroupAmount teams maxPerGroup
    | amount == 0 = Nothing
    | reminder == 0 = find (>=amount) groupAmounts
    | otherwise = find (>amount) groupAmounts
    where
        (amount, reminder) = quotRem teams maxPerGroup


-- this could probably be done in a prettier way
splitToGroups :: [String] -> Int -> Int -> [[String]] -> [[String]]
splitToGroups (x:xs) amt cur groups
    | length groups < amt - 1 = splitToGroups xs amt 0 (groups++[[x]])
    | length groups < amt = splitToGroups (revFirst xs amt) amt 0 (groups++[[x]])
    | cur == amt - 1 && modTwo = splitToGroups xs amt 0 newList
    | cur == amt - 1 = splitToGroups (revFirst xs amt) amt 0 newList
    | otherwise = splitToGroups xs amt (cur + 1) newList
        where
            newList = insertItem groups cur x
            modTwo = mod (length (head groups)) 2 == 0
splitToGroups [] _ _ groups = groups

insertItem xs i e = let (ys,zs) = splitAt i xs in ys ++ [(xs!!i)++[e]] ++ tail zs

revFirst xs i = reverse (take i xs)++drop i xs

recursive :: [[String]] -> [[Matchup]] -> [[Matchup]]
recursive (x:xs) a = recursive xs (getScheduleForGroup x:a)
recursive [] a = a

getScheduleForGroup :: [String] -> [Matchup]
getScheduleForGroup t
    | length t > 3 = getScheduleRec t []
    | otherwise = getScheduleRec t [] ++ getScheduleRec (reverse t) []

getScheduleRec :: [String] -> [Matchup] -> [Matchup]
getScheduleRec (x:xs) a = getScheduleRec xs (createMatchups x xs [] ++ a)
getScheduleRec [] a = a

createMatchups :: String -> [String] -> [Matchup] -> [Matchup]
createMatchups c (t:ts) a = createMatchups c ts (Matchup c t:a)
createMatchups c [] a = a



-- test code
testData7 = [
    (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), 
    (2,3), (2,4), (2,5), (2,6), (2,7), 
    (3,4), (3,5), (3,6), (3,7), 
    (4,5), (4,6), (4,7), 
    (5,6), (5,7), 
    (6,7)]


testData6 = [
    (1,2), (1,3), (1,4), (1,5), (1,6), 
    (2,3), (2,4), (2,5), (2,6), 
    (3,4), (3,5), (3,6), 
    (4,5), (4,6),
    (5,6)]

testData5 = [
    (1,2), (1,3), (1,4), (1,5),
    (2,3), (2,4), (2,5),
    (3,4), (3,5), 
    (4,5)]

perms = permutations testData6
matches = map (map (\(h,a) -> Matchup (show h) (show a))) perms
--testMatches = take 10000000000 (drop 12000000000 matches) -- testData7
testMatches = take 10000000000 (drop 11000000000 matches) -- testData6
--testMatches = matches -- testData5



test :: [[Matchup]]
--test = balanceMatches testMatches
test = balanceMatches 10 testMatches




