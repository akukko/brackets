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

    putStrLn "groups"
    mapM_ uprint groups  
    
    putStrLn "\nmatches"
    let schedule = getSchedule groups

    -- aja testikoodi
    --let schedule = test
    
    g schedule (map show (take (length schedule) [1,2..]))

f c xs  = putStrLn ("\n" ++ c ++ ":\n" ++ concat (intersperse "\n" $ map ushow xs))
g xss cs = zipWithM_ f cs xss