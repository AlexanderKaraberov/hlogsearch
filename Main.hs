module Main where

import System.FileSearch (searchLog, readLogLine)
import Data.Time.UTCTimes (mkUTCTime)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime (UTCTime))

main :: IO ()
main = do
    -- Let's assume we need to examine a very narrow window in an old and very large log file (50GB in size).
    
    -- set search parameters (N.B. naturally these should be passed as CLI args).
    -- log date we are looking for, or something close to it if this exact timestamp does not exist.
    let searchTerm = mkUTCTime (2019, 12, 12) (10, 34, 28.909266) :: UTCTime
    -- log file to use for search
    let path = "/var/log/cassandra/gc.log"
    -- now perform a search. 
    -- (if you need a more precise than 1 millisecond resolution, then consider using searchLog').
    result <- searchLog path searchTerm
    -- unpack search result
    let (found, offset) = fromMaybe (searchTerm, 0) result
    -- print a found log entry
    entry <- readLogLine path offset
    putStrLn $ "Entry with time: " ++ show found ++ " at offset: " ++ show offset ++ " is: \n" ++ entry
