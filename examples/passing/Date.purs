module Main where

  import Prelude
  import Maybe
  import Eff
  import Trace
  import Date

  main = do
    d <- now
    Trace.print d
    Trace.print $ parse "Tue Feb 18 2014 23:24:53 GMT+3000"
    Trace.print $ date 2003 January 01
    Trace.print $ dateTime 2044 February 01 13 26 48 06
    
    Trace.print $ toYear d
    Trace.print $ toMonth d
    Trace.print $ toDay d
    Trace.print $ toHours d
    Trace.print $ toMinutes d
    Trace.print $ toSeconds d
    Trace.print $ toMilliseconds d
    
    Trace.print $ toUTCYear d
    Trace.print $ toUTCMonth d
    Trace.print $ toUTCDay d
    Trace.print $ toUTCHours d
    Trace.print $ toUTCMinutes d
    Trace.print $ toUTCSeconds d
    Trace.print $ toUTCMilliseconds d
    
    Trace.print $ timezoneOffset d
    Trace.print $ toEpochMilliseconds d
    
    Trace.print $ fromEpochMilliseconds 0
    Trace.print $ fromEpochMilliseconds 999999999999
    Trace.print $ fromEpochMilliseconds (0-999999999999)