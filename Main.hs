module Main where

import System.File.Search (searchLog, readLogLine)
import Data.Time.UTCTimes (safelyParseISO8601)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime (UTCTime), NominalDiffTime)
import Options.Applicative
import Data.Semigroup ((<>))

-- Define a parser for CLI arguments using applicative functors
-- http://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html
data CLIArgs = CLIArgs
  { path      :: String
  , datetime  :: String
  , precision :: String }

cliArgs :: Parser CLIArgs
cliArgs = CLIArgs
      <$> strOption
          ( long "path"
         <> short 'p'
         <> metavar "/path/to/log_file"
         <> help "Path to the log file" )
      <*> strOption
          ( long "datetime"
         <> short 'd'
         <> metavar "YYYY-MM-DDTHH:MM:SS.ssssssZ"
         <> help "ISO 8601 datetime" )
      <*> strOption
        ( long "precision"
        <> metavar "0.00001"
        <> help "log entry time precision (maximum 1 picosecond)" 
        <> showDefault
        <> value "0.0001")

main :: IO ()
main = performSearch =<< execParser opts
  where
    opts = info (cliArgs <**> helper)
      ( fullDesc
     <> progDesc "Search for a log entry with DATETIME in PATH")

performSearch :: CLIArgs -> IO ()
performSearch (CLIArgs path datetime prec) = do
    -- Convert params
    let searchTerm = safelyParseISO8601 datetime
    let precision = realToFrac (read prec :: Float)
    -- Effectively perform a search
    result <- searchLog path searchTerm precision
    -- unpack search result -
    let (found, offset) = fromMaybe (searchTerm, 0) result
    -- print a found log entry
    entry <- readLogLine path offset
    putStrLn $ "Entry with time: " ++ show found ++ " at offset: " ++ show offset ++ " is: \n" ++ entry
