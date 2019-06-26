module Brackets (
    getPlayoffTeams) where


import Scoreboard

import Data.List

-- i is the amount of progressing teams per group.

getPlayoffTeams :: Int -> [[TeamScore]] -> [TeamScore]
getPlayoffTeams i ts = sortBy betterAverage (concatMap (take i) ts)

betterAverage ts1 ts2 = compare (averageScore ts2) (averageScore ts1)

