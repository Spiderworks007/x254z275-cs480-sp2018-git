-- Dishank Patel
-- X@%$Z@&%(x254z275)
-- ha6.hs

------------------------ elem Function-------------------
elem':: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs)
  | y == x = True
  | otherwise = elem' y xs

---------------------toLower Function--------------------------------
toLower :: String -> String
toLower[] = []
toLower(x:xs)
  |not(elem' x ['A'..'Z']) = x:toLower xs
  |otherwise = [x..]!!32 : toLower xs

----------------------Break Function----------------------------------
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ xs@[] = (xs, xs)
break' p xs@(x:xs')
           | p x = ([],xs)
           | otherwise = let (ys,zs) = break' p xs' in (x:ys,zs)

-----------------------ha5 String to Word Function-----------------------
s2w :: String -> [String]
s2w []  = []
s2w xxs@(x:xs) 
  | x == ' '  = s2w xs
  | otherwise = ys : s2w rest
                  where  (ys, rest) = break' (== ' ') xxs

-------------------------rec Function-------------------------------------                  
rec' ::[String] -> [String] -> Integer
rec' [] _ = 0
rec' (x:xs) (y:ys)
  |toLower x == toLower y = (count' (x) + (rec' xs ys))
  |otherwise = (rec' xs ys)

------------------------------- Reverse Function ------------------------------

reve' :: [a] -> [a]
reve' [] = []
reve' (x:xs) = reve' xs ++ [x]
----------------------------length function----------------------------
len' :: String -> Integer
len' "" = 0
len' (_:xs) = 1 + len' xs

----------------------------------Count function --------------------
count' :: String -> Integer
count' xs = len'[ x | x <- xs , x == 'a'|| x == 'e'||x == 'i'||x == 'o'|| x == 'u'|| x == 'A'|| x == 'E'||x == 'I'||x == 'O'|| x == 'U'] 


--------------------------------- Main ------------------------------------------
main :: IO()
main = do
  line <- prompt "Text: "
  line <- getLine
    if line == null
      then return()
    else do
      let reve'Input = reve'(s2w(reve' line))
      let input = s2w line
      print (rec' input reve'Input)
      main

      