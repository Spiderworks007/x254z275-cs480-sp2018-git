-- DIshank Patel
-- X @ % $ Z @ & % (x254z275)

dos :: Int -> Int
dos n
    | (n == 1) = 1
    | (n <= 0) = 0
    | (n > 1) = (((sum[1..n])^2) - ((n*(n+1)*(2*n+1)) `div`6 ))

s2w :: String -> [String]
s2w []  = []
s2w xxs@(x:xs) 
  | x == ' '  = s2w xs
  | otherwise = ys : s2w rest
                  where (ys, rest) = break (== ' ') xxs