import Test.QuickCheck
import Data.Char

import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Data.Maybe
import System
import Data.List
import Data.List.Split
import Data.Function

take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs
prop_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs

bfClosest :: (Ord a, Floating a) => [(a, a)] -> Maybe ((a, a), (a, a))
bfClosest pairs = 
	snd $ foldl (\ acc@(min, soFar) (p1, p2) -> 
					if distance p1 p2 < min then (distance p1 p2, Just(p1, p2)) else acc) 
		        (fromIntegral (maxBound :: Int), Nothing) 
		        [(pairs !! i, pairs !! j) | i <- [0..length pairs - 1], j <- [0..length pairs-1 ], i /= j]
    where distance (x1, y1) (x2, y2) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

type Point a = (a, a)

data Pair a = P (Point a) (Point a) deriving (Show)

instance (Eq a) => Eq (Pair a) where
	P a b == P c d = a == c && b == d || a == d && b == c


dcClosest :: (Ord a, Floating a) => [Point a] -> (Point a, Point a)
dcClosest pairs
	| length pairs <= 3 = fromJust $ bfClosest pairs    
	| otherwise = foldl (\closest (p1:p2:_) -> minimumBy (compare `on` distance) [closest, (p1, p2)])
	                    closestPair 
	                    (windowed 2 pairsWithinMinimumDelta)
	where sortedByX = sortBy compare pairs	      
	      (leftByX:rightByX:_) = chunk (length sortedByX `div` 2) sortedByX	      	      
	      closestPair = minimumBy (compare `on` distance) [closestLeftPair, closestRightPair]
	      	where closestLeftPair =  dcClosest leftByX
	              closestRightPair = dcClosest rightByX	 	      	        
	      pairsWithinMinimumDelta = sortBy (compare `on` snd) $ filter withinMinimumDelta sortedByX
	        where withinMinimumDelta (x, _) = abs (xMidPoint - x) <= distance closestPair	
	                where (xMidPoint, _) = last leftByX

distance :: Floating a => (Point a, Point a) -> a
distance ((x1, y1), (x2, y2)) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)	  

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then (take size ls) : windowed size xs else windowed size xs	

prop_closest_pairs xs = length xs >= 2 ==> dcClosest xs == (fromJust $ bfClosest xs)

prop_groups_adjacent_items n xs = n < length xs ==>  
								  not (null xs) ==>
								  n > 0 ==>
								  	(last $  last $ windowed n xs) == last xs

prop_nothing_if_n_too_large n xs = n > length xs ==>  windowed n xs == []

prop_size_of_sub_arrays_is_n n xs =  n > 0 ==> all (\subArray -> length subArray == n)  (windowed n xs)