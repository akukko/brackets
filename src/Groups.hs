module Groups (
    calculateGroupAmount,
    getGroups) where

import Matchup
import Data.List

{- 

Takes a list of teams, and the max number of teams per group.
Returns a list of groups.

Fills the groups evenly so that the number of groups is a power of two.

-}

getGroups :: [String] -> Int -> [[String]]
getGroups s max = case groupAmount of
    Just amt -> splitToGroups s amt 0 []
    Nothing -> []
    where
        groupAmount = calculateGroupAmount (length s) max

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
