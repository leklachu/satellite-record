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

data Satellite = Terra | Aqua
               deriving Eq
suffix Terra = ".jpg"
suffix Aqua = "a.jpg"

checkYear year satellite = mapM doesFileExist paths >>=
                           print.and
  where paths = map (path "image-directory" (suffix satellite)) $ theYear year

-- prepare year needs createDirectory and doesDirectoryExist. or just createDirectoryIfMissing
-- also, really should be catching errors!
-- also want to check for null files.

check satellite date = doesFileExist $ path "image-directory" (suffix satellite) date

