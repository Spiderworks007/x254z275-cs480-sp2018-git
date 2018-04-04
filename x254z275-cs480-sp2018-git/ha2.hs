--Dishank Patel
-- Classify and Permutation 
-- Sample Input and Output formate as follow
-- *Main> perms [(0, 1), (1, 2), (2, 3)]   [(2, 3), (0, 1), (1, 2)]
--True
-- *Main> classify 28
-- "perfect"
import Data.List 
-- fl first list 
-- sl second list 
perms :: Eq a => [a] -> [a] -> Bool
perms [] [] = True
perms fl (s:sl) | not(length fl == length (s:sl)) = False
                | otherwise = perms (delete s fl) sl

classify :: Int -> [Char]
classify n =
    if n == 0
        then 
        "Illegal"
    else if 
        sum [x | x <- [1..n], n `mod` x == 0, x /= n] > n
        then 
         "Abundant"
    else if 
        sum [x | x <- [1..n], n `mod` x == 0, x /= n] < n
        then 
        "Deficient"
   else
        "perfect"