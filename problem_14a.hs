import Data.List

problem_14 = j 1000000 where   
f :: Int -> Integer -> Int   
f k 1 = k   
f k n = f (k+1) $ if even n then div n 2 else 3*n + 1   
g x y = if snd x < snd y then y else x   
h x n = g x (n, f 1 n)   
j n = fst $ foldl' h (1,1) [2..n-1]

main :: IO ()
main = do
  print (problem_14)   