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

    printSchedule $ getSchedule groups

    -- aja testikoodi
    -- printSchedule $ test
    
    

printSchedule s = g s (map show (take (length s) [1,2..])) 
g xss cs = zipWithM_ f cs xss
f c xs  = putStrLn ("\n" ++ c ++ ":\n" ++ concat (intersperse "\n" $ map ushow xs))