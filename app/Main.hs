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


main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args!!1)
    putStrLn ""
    let filtered = readTeams $ lines contents
    let groups = getGroups filtered (read (head args) :: Int)

    putStrLn "groups"
    mapM_ uprint groups

    putStrLn "\nmatches"

    let schedule = getSchedule groups 10

    -- printSchedule schedule
    
    --writeSchedule "schedule.txt" schedule

    file <- readFile "schedule.txt"
    
    print $ readResults file

    -- aja testikoodi
    -- printSchedule test
    --print $ show (length test)


printSchedule s = g s (map show (take (length s) [1,2..]))
g xss cs = zipWithM_ f cs xss
f c xs  = putStrLn ("\n" ++ c ++ ":\n" ++ concat (intersperse "\n" $ map ushow xs))