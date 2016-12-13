module Main where

import FileOps
import Dates

-- Satellites are Terra (crosses N/S in morning) and Aqua (crosses S/N in afternoon)

satelliteURL day year satellite =
  "http://map2.vis.earthdata.nasa.gov/imagegen/index.php?TIME="
  ++ show year ++ day'
  ++ "&extent=82.357421875,28.582763671876,83.717529296875,29.760498046876&epsg=4326&layers="
  ++ layer
  ++ "&format=image/jpeg&width=619&height=536"
  where day' | day <  10 = "00" ++ show day
             | day < 100 = "0"  ++ show day
             | otherwise = show day
        layer = if satellite == Terra
                then "MODIS_Terra_CorrectedReflectance_TrueColor"
                else "MODIS_Aqua_CorrectedReflectance_TrueColor"

process satellite date = do got <- check satellite date
                            if got then hooray
                              else get satellite date
  where hooray = print $ show date ++ " - is already there"

get satellite date = do putStr $ show date ++ " - getting..."
                        -- ...
                        putStrLn "...got."

processYear year = mapM (process Terra) (theYear year) >> mapM (process Aqua) (theYear year)

main = processYear 2016
