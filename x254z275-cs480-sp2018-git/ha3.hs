-- Dishank Patel 
--ha3.hs
--binary to Decimal.
-- addTwos adds two double in a list where a is first element and b is second element inside a list.
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

binInt :: Integral a => String -> Maybe a
binInt = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

addTwos:: [Double] -> [Double]
addTwos   [x]= []
addTwos   [] = []
addTwos (a:b:x) = [a+b] ++ addTwos x

fun7 :: [Int] -> ([Int],[Int])
fun7 lst = fun7Help lst True
fun7Help :: [Int] -> Bool -> ([Int],[Int])
fun7Help [] _ = ([],[])
fun7Help (h:t) b
    |b = (h:f,s)
    |otherwise = (f,h:s)
    where f = fst reset
          s = snd reset
          reset = fun7Help t (not b) 
