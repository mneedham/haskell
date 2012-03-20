import Data.List

floorDiv :: Integral a => a -> a -> a
floorDiv x y = x `div` y

next :: Int -> Int
next n = if n `mod` 2 == 0 then floorDiv n 2 else (n * 3) + 1

seqFor :: Int -> [Int]
seqFor n = if n == 1 then [1] else n : seqFor (next n)

compareSnd (_, y1) (_, y2) = compare y1 y2


problem_14a n = maximumBy compareSnd $ zip [1..n] $ map (length . seqFor) [1..n]
problem14 = fst $ problem_14a 999999

main :: IO ()
main = do
  print (problem14)   	