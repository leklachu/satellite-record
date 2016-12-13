-- Dates

module Dates where

{-
years = map show [2013..]
months = map textNorm [1..12]
days = map textNorm [1..]
-}

textNorm :: Int -> String
textNorm i | i < 10 = '0' : show i
           | otherwise = show i

isLeapYear 2012 = True
isLeapYear 2013 = False
isLeapYear 2014 = False
isLeapYear 2015 = False
isLeapYear 2016 = True
isLeapYear 2017 = False
isLeapYear y | mod y 400 == 0 = True
             | mod y 100 == 0 = False
             | mod y 4   == 0 = True
             | otherwise      = False

monthLength m y | isLeapYear y && m == 2 = 29
                | otherwise = [31,28,31,30,31,30,31,31,30,31,30,31] !! (m-1)

data Date = Date {day :: Int
                 ,month :: Int
                 ,year :: Int
                 ,daynum :: Int
                 }
inc (Date d m y i) | m == 12 && d == 31 = Date 1 1 (y+1) 1
                   | monthEnd  = Date 1 (m+1) y (i+1)
                   | otherwise = Date (d+1) m y (i+1)
  where monthEnd = monthLength m y == d

instance Show Date where
  show (Date d m y i) = textNorm d ++ "/" ++ textNorm m ++ "/" ++ show y ++ " - day " ++ show i

theYear year = take ylen $ iterate inc (startIn year)
  where ylen = if isLeapYear year then 366 else 365


startDate = Date 8 5 2012 129
startIn y = Date 1 1 y 1
showDates n = mapM_ (putStrLn.show) $ take n $ iterate inc startDate
showOf y = mapM_ (putStrLn.show) $ take 366 $ iterate inc (startIn y)

