module Main where

import Groups
import Schedule
import Matchup
import Result
import ReadTeams
import ReadResults
import Write

import System.Environment
import Data.List
import Data.Ord
import Control.Monad
import Text.Show.Unicode
import System.IO
import System.Random.Shuffle
import Data.Maybe

userData s = "userData/" ++ s ++ ".txt"
scheduleFile = userData "schedule"
teamsFile = userData "teams"
maxTeamFile = userData "maxTeams"

main :: IO ()
main = do
    args <- getArgs
    (groups, schedule) <- readInfo args

    let maxTeamNameLength = length (maximumBy (comparing length) (concat groups))
    let pr = prettyResult maxTeamNameLength
    file <- readFile scheduleFile

    let prettySchedule = groupedInfo (readResults file) pr
    let prettyGroups = groupedInfoLabels groups "Group " id

    putStrLn (concat (zipWith (\a b -> a ++ b ++ "\n") prettyGroups prettySchedule))

    -- aja testikoodi
    -- printSchedule test
    --print $ show (length test)

groupedInfoLabels s gr = g s (map ((\x ->"\n"++gr++x++":\n"++concat (replicate 8 "=")++"\n\n") . show) (take (length s) [1,2..]))
groupedInfo s = g s (replicate (length s) "\n\n")
g xss cs fun = zipWith (f fun) xss cs
f fun xs c = c ++ intercalate "\n" (map fun xs)

getShuffled ts 0 = return ts
getShuffled (t:ts) i = do
    let first = take i (t:ts)
    shuffled <- shuffleM first
    end <- getShuffled (drop i (t:ts)) i
    return (shuffled ++ end)
getShuffled [] i = return []

readInfo args = do
    let teamPath = if length args > 1 then args!!1 else teamsFile

    let newGen = teamPath /= teamsFile

    ts <- readFile teamPath
    let teams = readTeams $ lines ts

    max <- if newGen
        then return (read (head args) :: Int)
        else do
            file <- readFile maxTeamFile
            return (read file :: Int)

    let groupAmount = fromMaybe 0 (calculateGroupAmount (length teams) max)

    teams <- getShuffled teams groupAmount

    let groups = getGroups teams groupAmount
    let schedule = getSchedule groups 10

    when newGen $ do
        writeTeams teamsFile teams
        writeSchedule scheduleFile schedule
        writeMaxTeamAmount maxTeamFile max

    return (groups, schedule)
