module WriteSchedule (
    writeSchedule) where

import Matchup

import Data.List

writeSchedule :: String -> [[Matchup]] -> IO ()
writeSchedule path ms = do
    let contents = intercalate "\n" (map (writeGroup "") ms)
    writeFile path contents

writeGroup = foldl (\r m -> r ++ (home m ++ ", " ++ away m ++ ", , \n"))

