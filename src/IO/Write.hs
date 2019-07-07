module IO.Write (
    writeSchedule,
    writeTeams,
    writeMaxTeamAmount) where

import Result

import Data.List

writeSchedule :: String -> [[Result]] -> IO ()
writeSchedule path rs = writeFile path (intercalate "\n" (map (writeGroup "") rs))

writeGroup :: String -> [Result] -> String
writeGroup = foldl buildLine

buildLine cur r = cur ++ (scoreH ++ "," ++ scoreA ++ "," ++ home r ++ ", " ++ away r ++ "\n")
    where 
        scoreH = getScore (homeScore r)
        scoreA = getScore (awayScore r)

getScore s = case s of 
    Just i -> show i
    Nothing -> ""

writeTeams :: String -> [String] -> IO ()
writeTeams path ts = writeFile path (intercalate "\n" ts)

writeMaxTeamAmount :: String -> Int -> IO ()
writeMaxTeamAmount path max = writeFile path (show max)
