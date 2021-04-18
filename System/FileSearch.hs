{-# LANGUAGE MultiWayIf #-}

module System.FileSearch (
    searchLog,
    searchLog',
    searchLogUntil,
    readLogLine
) where

import System.IO
    ( hSeek,
      openFile,
      hGetLine,
      SeekMode(AbsoluteSeek),
      IOMode(ReadMode) )
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime (UTCTime), diffUTCTime, NominalDiffTime)
import Data.Time.UTCTimes (compareLogTimes, mkUTCTime, zeroUTCJulianDay)
import System.PosixCompat (getFileStatus)
import System.PosixCompat.Files (fileSize)
import Data.Time.ISO8601 ( parseISO8601 )

-- Search a log file with 1 millisecond precision searching the whole log file until eof.
searchLog :: String -> UTCTime -> IO (Maybe (UTCTime, Integer))
searchLog file key = do 
    fileSize <- getFileSize file
    -- assume one millisecond log precision as a sensible default.
    let precision = 0.001
    searchLogUntil file key fileSize precision

-- Same as searchLog but allow to specify a custom log time precision.
searchLog' :: String -> UTCTime -> NominalDiffTime -> IO (Maybe (UTCTime, Integer))
searchLog' file key precision = do 
    fileSize <- getFileSize file
    searchLogUntil file key fileSize precision

-- A more customizable variant of searchLog with additional params notably:
-- high : defines an arbitrary offset to use as a search boundary.
-- precision: defines a custom resolution used to compare log entries' timestamps.
searchLogUntil :: [Char] -> UTCTime -> Integer -> NominalDiffTime -> IO (Maybe (UTCTime, Integer))
searchLogUntil file key high precision =
    go file key 0 high where
        go _ _ low high = do
            let mid = (high + low) `div` 2
            pivot <- getLogTime file mid
            if | high < low -> return Nothing 
               | compareLogTimes pivot key precision == GT -> go file key low (mid - 1)
               | compareLogTimes pivot key precision == LT -> go file key (mid + 1) high
               | otherwise -> return $ Just (pivot, mid)

-- Read a line from a file specified via path at a given offset.          
readLogLine :: FilePath -> Integer -> IO String
readLogLine p offset = do 
    hdl <- openFile p ReadMode
    hSeek hdl AbsoluteSeek offset
    l <- hGetLine hdl
    let diff = toInteger $ length l + 1
    hSeek hdl AbsoluteSeek (diff + offset)
    hGetLine hdl

-- Internal auxiliary functions

getLogTime :: FilePath -> Integer -> IO UTCTime
getLogTime = (utcTimeFromLog .) . readLogLine

utcTimeFromLog :: IO String -> IO UTCTime 
utcTimeFromLog log = do 
                    logEntry <- log
                    let _:time:_ = words logEntry
                    return $ fromMaybe zeroUTCJulianDay $ parseISO8601 time

getFileSize :: String -> IO Integer
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)
