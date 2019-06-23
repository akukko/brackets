module Write (
    writeSchedule,
    writeTeams,
    writeMaxTeamAmount) where

import Matchup

import Data.List

writeSchedule :: String -> [[Matchup]] -> IO ()
writeSchedule path ms = writeFile path (intercalate "\n" (map (writeGroup "") ms))

writeGroup = foldl (\r m -> r ++ (home m ++ ", " ++ away m ++ ", , \n"))

writeTeams :: String -> [String] -> IO ()
writeTeams path ts = writeFile path (intercalate "\n" ts)

writeMaxTeamAmount :: String -> Int -> IO ()
writeMaxTeamAmount path max = writeFile path (show max)
