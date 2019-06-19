module Brackets ( 
    getGroups,
    getSchedule,
    test) where

import Matchup
import Balance

sameTeam :: Matchup -> Matchup -> Bool
sameTeam x y
    | away x == away y = True
    | away x == home y = True
    | home x == away y = True
    | home x == home y = True
    | otherwise = False

-- Takes a list of teams, and the number of teams per group.
-- Returns a list of groups.
getGroups :: [String] -> Int -> [[String]]
getGroups s max = splitToGroups s max [] []

-- Takes a list of groups. 
-- Returns a list of group's matchups.
getSchedule :: [[String]] -> [[Matchup]]
getSchedule g = balanceMatches $ recursive g []

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

splitToGroups :: [String] -> Int -> [String] -> [[String]] -> [[String]]
splitToGroups (x:xs) max c w
    | mod (length xs) max /= 0 && length c == max - 2 = splitToGroups xs max [] ((x:c):w)
    | length c == max - 1 = splitToGroups xs max [] ((x:c):w)
    | otherwise = splitToGroups xs max (x:c) w
splitToGroups [] max c w = c:w






test :: Int -> [[Matchup]]
test _ = balanceMatches [[
                    Matchup "1" "2",
                    Matchup "1" "3",
                    Matchup "1" "4",
                    Matchup "2" "3",
                    Matchup "4" "3",
                    Matchup "2" "4"]]

{-
getGroups :: [String] -> String
getGroups s = render $ vsep 1 left boxes
        where boxes = map (\z -> text z) s 
-}


