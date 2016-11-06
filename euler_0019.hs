import Data.Dates

main = print $ length $ filter (== Sunday)
     $ map dateWeekDay [DateTime y m 1 0 0 0 | y<-[1901 .. 2000], m<-[1..12]]