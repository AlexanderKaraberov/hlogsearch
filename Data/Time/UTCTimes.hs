module Data.Time.UTCTimes (
   compareLogTimes,
   mkUTCTime,
   zeroUTCJulianDay
) where 

import Data.Time.Clock (UTCTime (UTCTime), diffUTCTime, NominalDiffTime)
import Data.Time.ISO8601 ( parseISO8601 )
import Data.Fixed (Pico)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime, ZonedTime, timeOfDayToTime, TimeOfDay(TimeOfDay))

-- Compare log entries timestamps using an arbitrary (up to a microsecond) precision
compareLogTimes :: UTCTime -> UTCTime -> NominalDiffTime -> Ordering
compareLogTimes fst snd epsilon
    | diff > epsilon = GT
    | diff < epsilon && diff > (-epsilon) = EQ
    | diff < (-epsilon) = LT
    where diff = diffUTCTime fst snd

-- Construct UTC time
mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

-- Constructs a zero Julian day as UTCTime type. 
-- You may use this if you need a more or less reasonable default when working with Maybe UTCTime.
-- It uses a VAX/VMS base time (November 17 1858) as a zero day.
-- http://shanebow.com/page/show/julian-day 
zeroUTCJulianDay :: UTCTime
zeroUTCJulianDay = mkUTCTime (1858, 11, 17) (0, 0, 0.000000)
