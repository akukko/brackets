module Main where

import Brackets
import ReadTeams

import System.Environment
import Data.List
import Control.Monad
import Text.Show.Unicode


main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args!!1)
    putStrLn ""
    let filtered = readTeams $ lines contents
    let groups = getGroups filtered (read (head args) :: Int)
    -- putStrLn "all teams"        
    -- (mapM_ putStrLn) filtered
    putStrLn "groups"
    mapM_ print groups  
    
    putStrLn "\nmatches"
    let schedule = getSchedule groups

    --let schedule = test 1
    
    g schedule (map show (take (length schedule) [1,2..]))




    


    -- mapM_ (mapM_ print) schedule
    -- print $ getScheduleForGroup (head groups)

f c xs  = putStrLn ("\n" ++ c ++ ":\n" ++ concat (intersperse "\n" $ map ushow xs))
g xss cs = zipWithM_ f cs xss