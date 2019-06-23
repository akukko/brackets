module Main where

import ReadTeams
import Groups
import Schedule
import Matchup
import ReadResults
import WriteSchedule

import System.Environment
import Data.List
import Control.Monad
import Text.Show.Unicode
import System.IO
import System.Random.Shuffle
import Data.Maybe

userData s = "userData/" ++ s ++ ".txt"

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args!!1)

    let teams = readTeams $ lines contents
    putStrLn ""
    let maxPerGroup = read (head args) :: Int
    let groupAmount = calculateGroupAmount (length teams) maxPerGroup


    shuffledTeams <- getShuffled teams (fromMaybe 0 groupAmount)
    let groups = getGroups shuffledTeams maxPerGroup

    putStrLn "groups"
    mapM_ uprint groups

    putStrLn "\nmatches"
    let scheduleFile = userData "schedule"
    let schedule = getSchedule groups 10

    -- printSchedule schedule
    writeSchedule scheduleFile schedule

    file <- readFile scheduleFile

    printSchedule $ readResults file
    print ""
    -- aja testikoodi
    -- printSchedule test
    --print $ show (length test)


printSchedule s = g s (map show (take (length s) [1,2..]))
g xss cs = zipWithM_ f cs xss
f c xs  = putStrLn ("\n" ++ c ++ ":\n" ++ concat (intersperse "\n" $ map ushow xs))

getShuffled (t:ts) i = do
    let first = take i (t:ts)
    shuffled <- shuffleM first
    end <- getShuffled (drop i (t:ts)) i
    return (shuffled ++ end)
getShuffled [] i = return []


