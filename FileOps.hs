-- File operations for the satellite record creator

module FileOps where

import System.Directory
import Dates
import Control.Monad (liftM)

years = map show [2013..]
months = map textNorm [1..12]
days = map textNorm [1..]

path prefix suffix (Date d m y i) =
  prefix ++ "/" ++ show y ++ "/" ++ textNorm m ++ "/" ++ textNorm d ++ suffix

checkGot from = mapM_ (\x -> doesFileExist x >>= print) $ map (path "image-directory" ".jpg") $ take 366 $ iterate inc (startIn from)

