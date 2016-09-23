import qualified Data.ByteString as B
import Network.HTTP
import Network.URI (parseURI)
import Data.List (foldl')

picu = "http://map2.vis.earthdata.nasa.gov/imagegen/index.php?TIME=2014004&extent=82.403564453125,28.743164062501,83.69775390625,29.738525390626&epsg=4326&layers=MODIS_Terra_CorrectedReflectance_TrueColor,Coastlines&format=image/jpeg&width=589&height=453"

picf = "/home/x/multimedia/pictures/dolpa satellite/yep.jpg"

puti url filename = do
  jpg <- get url
  B.writeFile filename jpg
  where get u = let uri = case parseURI u of
                      Nothing -> error $ "Invalid URI: " ++ u
                      Just u -> u
                in simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

--- Date calculation ---
monthLengthNorm = [31,28,31,30,31,30,31,31,30,31,30,31]
monthLengthLeap = [31,29,31,30,31,30,31,31,30,31,30,31]
isLeapYear 2012 = True
isLeapYear 2013 = False
isLeapYear 2014 = False
isLeapYear 2015 = False
isLeapYear 2016 = True

monthDay d m ms = if d <= monthDays then (d,m)
               else monthDay (d - monthDays) (m+1) (tail ms)
  where monthDays = head ms

day date month year = sum (take (month-1) ms) + date
  where ms | isLeapYear year = monthLengthLeap
           | otherwise       = monthLengthNorm

date day year = monthDay day 1 ms
  where ms | isLeapYear year = monthLengthLeap
           | otherwise       = monthLengthNorm

aYear year | isLeapYear year = [1..366]
           | otherwise       = [1..365]

days 2012 = [129..366]
days 2013 = [1..365]
days 2014 = [1..325]

--- url & filename ---
-- main is Terra (crosses N/S in morning)
satelliteURL day year = "http://map2.vis.earthdata.nasa.gov/imagegen/index.php?TIME="
                        ++ show year ++ day'
                        ++ "&extent=82.357421875,28.582763671876,83.717529296875,29.760498046876&epsg=4326&layers=MODIS_Terra_CorrectedReflectance_TrueColor&format=image/jpeg&width=619&height=536"
  where day' | day <  10 = "00" ++ show day
             | day < 100 = "0"  ++ show day
             | otherwise = show day
--satellitePath day year = "/home/x/multimedia/pictures/dolpa satellite/"
satellitePath day year = "image-directory/"
                         ++ show year ++ "/" ++ jot month ++ "/" ++ jot d
                         ++ ".jpg"
  where (d,month) = date day year
        jot n | n <  10 = '0' : show n
              | n >= 10 = show n

-- secondary (a) is Aqua (S/N, afternoon)
satelliteURLa day year = "http://map2.vis.earthdata.nasa.gov/imagegen/index.php?TIME="
                         ++ show year ++ day'
                         ++ "&extent=82.357421875,28.582763671876,83.717529296875,29.760498046876&epsg=4326&layers=MODIS_Aqua_CorrectedReflectance_TrueColor&format=image/jpeg&width=619&height=536"
  where day' | day <  10 = "00" ++ show day
             | day < 100 = "0"  ++ show day
             | otherwise = show day
--satellitePatha day year = "/home/x/multimedia/pictures/dolpa satellite/"
satellitePatha day year = "image-directory/"
                          ++ show year ++ "/" ++ jot month ++ "/" ++ jot d
                         ++ "a.jpg"
  where (d,month) = date day year
        jot n | n <  10 = '0' : show n
              | n >= 10 = show n

--- get the stuff! ---

getIt day year = puti (satelliteURL day year) (satellitePath day year)
getEm ds y = mapM_ (\d -> getIt d y >>
                          let (dy,month) = date d y
                          in putStrLn ("Got " ++ show y ++ "/" ++ jot month
                                       ++ "/" ++ jot dy)
                   ) ds
  where jot n | n <  10 = '0' : show n
              | n >= 10 = show n

getEmAll = getEm (days 2012) 2012 >> getEm (days 2013) 2013 >>
           getEm (days 2014) 2014

getIta day year = puti (satelliteURLa day year) (satellitePatha day year)
getEma ds y = mapM_ (\d -> getIta d y >>
                           let (dy,month) = date d y
                           in putStrLn ("Got " ++ show y ++ "/" ++ jot month
                                        ++ "/" ++ jot dy
                                        ++ "  -- Aqua")
                    ) ds
  where jot n | n <  10 = '0' : show n
              | n >= 10 = show n

getEmAlla = getEma (days 2012) 2012 >> getEma (days 2013) 2013 >>
            getEma (days 2014) 2014


getE = getIt 129 2012 >> getIt 130 2012 >> getIt 131 2012

---------
---------

