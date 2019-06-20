module Brackets (
    getGroups,
    getSchedule,
    test) where

import Matchup
import Balance
import Data.List

-- Takes a list of teams, and the number of teams per group.
-- Returns a list of groups.
getGroups :: [String] -> Int -> [[String]]
getGroups s max = case groupAmount of
    Just amt -> splitToGroups s amt 0 []
    Nothing -> []
    where
        groupAmount = calculateGroupAmount (length s) max

-- Takes a list of groups. 
-- Returns a list of group's matchups.
getSchedule :: [[String]] -> [[Matchup]]
getSchedule g = reverse $ balanceMatches $ recursive g []

groupAmounts = iterate (*2) 1

calculateGroupAmount :: Int -> Int -> Maybe Int
calculateGroupAmount teams maxPerGroup
    | amount == 0 = Nothing
    | reminder == 0 = find (>=amount) groupAmounts
    | otherwise = find (>amount) groupAmounts
    where
        (amount, reminder) = quotRem teams maxPerGroup



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
    | otherwise = getScheduleRec t [] ++ getScheduleRec t []

getScheduleRec :: [String] -> [Matchup] -> [Matchup]
getScheduleRec (x:xs) a = getScheduleRec xs (createMatchups x xs [] ++ a)
getScheduleRec [] a = a

createMatchups :: String -> [String] -> [Matchup] -> [Matchup]
createMatchups c (t:ts) a = createMatchups c ts (Matchup c t:a)
createMatchups c [] a = a



-- test code

testData = [(h,a) |
    h <- [1..3],
    a <- [1..3],
    h /= a]

combs = take 6 (permutations testData)

testMatches = map (map (\(h,a) -> (Matchup (show h) (show a)))) combs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..length xs-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]


test :: [[Matchup]]
test = balanceMatches testMatches




