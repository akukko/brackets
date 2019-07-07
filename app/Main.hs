module Main where

import Groups
import Schedule
import Matchup
import Result
import Scoreboard

import Brackets
import Format
import IO.ReadTeams
import IO.ReadResults
import IO.Write

import System.Environment
import Data.List
import Data.Ord
import Control.Monad
import Control.Exception
import Control.DeepSeq
import Text.Show.Unicode
import System.IO
import System.Random.Shuffle
import System.Directory
import Data.Maybe

import Debug.Trace

userDataDir = "data/"
userData s = userDataDir ++ s ++ ".txt"
scheduleFile = userData "groupStage"
teamsFile = userData "teams"
configFile = userData "config"
playoffFile = userData "playoffs"

main :: IO ()
main = do
    putStrLn "\n\n\n"
    args <- getArgs
    (groups, schedule) <- readInfo args

    let longest = length (maximumBy (comparing length) (concat groups))

    file <- readFile scheduleFile    

    let results = readResults file
    let prettySchedule = printableSchedule longest results 

    let scores = buildScoreboards results
    let prettyScores = printableScoreboards longest scores


    let groupsTitles = map (\x -> "Group " ++ show x ++ "\n") (take (length scores) [1,2..])
    putStrLn (concat $ combine "\n\n\n" (combine "" groupsTitles prettyScores) prettySchedule)


    let advance = 2
    let playoffTeams = getPlayoffTeams advance scores
    putStrLn "Playoff teams:"
    putStrLn $ head $ printableScoreboards longest [playoffTeams]
    
    playoffsExist <- doesFileExist playoffFile

    let calculatedBrackets = getBrackets $ getFirstRound advance scores

    oldBrackets <- if playoffsExist
        then do
            playoffs <- readFile playoffFile
            evaluate (force playoffs)
            let results = readResults playoffs
            if sameMatches (head results) (head calculatedBrackets)
                then return $ results
                else return calculatedBrackets
        else
            return calculatedBrackets

    let brackets = updateBrackets oldBrackets

    writeSchedule playoffFile brackets

    let playoffGames = printableSchedule longest brackets
    putStrLn (intercalate "" playoffGames)


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
    evaluate (force teams)
    
    max <- if newGen
        then return (read (head args) :: Int)
        else do
            file <- readFile configFile
            return (read file :: Int)

    let groupAmount = fromMaybe 0 (calculateGroupAmount (length teams) max)

    teams <- getShuffled teams groupAmount

    let groups = getGroups teams groupAmount
    let schedule = getSchedule groups 10

    when newGen $ do
        createDirectoryIfMissing False userDataDir
        writeTeams teamsFile teams
        writeSchedule scheduleFile schedule
        writeMaxTeamAmount configFile max

    return (groups, schedule)
