module Schedule (
    getSchedule,
    test,
    printableSchedule) where


import Matchup
import Result
import Balance
import Format

import Data.List

-- Takes a list of groups. 
-- Returns a list of groups' matchups.
getSchedule :: [[String]] -> Int -> [[Matchup]]
getSchedule g limit = reverse $ balanceMatches limit $ recursive g []

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
createMatchups c (t:ts) a = createMatchups c ts (matchup c t:a)
createMatchups c [] a = a


printableSchedule rs mt = groupedInfo rs (replicate (length rs) "\n\n") (prettyResult mt)




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




