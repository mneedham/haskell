import Test.QuickCheck
import Data.Char
import Debug.Trace

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

bfClosest :: (Ord a, Floating a) => [Point a] -> Maybe (Pair a)
bfClosest pairs = 
	snd $ last $ 
	    let x  = scanl (\ acc@(min, soFar) (p1, p2) -> 
			             if distance p1 p2 < min then (distance p1 p2, Just(P p1 p2)) else acc) 
		               (fromIntegral (maxBound :: Int), Nothing) 
		         [(pairs !! i, pairs !! j) | i <- [0..length pairs - 1], j <- [0..length pairs-1 ], i /= j] in
		x
    where distance (x1, y1) (x2, y2) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

type Point a = (a, a)

data Pair a = P (Point a) (Point a) 

toPair :: (Point a, Point a) -> Pair a
toPair (a,b) = P a b

instance (Eq a) => Eq (Pair a) where
	P a b == P c d = a == c && b == d || a == d && b == c

instance (Show a) => Show (Pair a) where
	show (P a b) = (show a) ++ " " ++ (show b)


combos :: [a] -> [(a,a)]
combos initial = 
	[(initial !! i, initial !! j) | i <- [0..length initial - 1], j <- [i+1..length initial-1 ], i /= j]

dcClosest :: (Ord a, Floating a) => [Point a] -> Pair a
dcClosest pairs  
	| length pairs <= 3 = 		 
		fromJust $ bfClosest pairs
	| otherwise = 		    
		last $ scanl (\closest (p1,p2) -> minimumBy (compare `on` distance') [closest, P p1 p2])
	           closestPair 
	           (combos pairsWithinMinimumDelta)
	where sortedByX = sortBy compare pairs	      
	      (leftByX:rightByX:_) = [take (length sortedByX `div` 2) sortedByX, drop (length sortedByX `div` 2) sortedByX]      
	      closestPair = minimumBy (compare `on` distance') [closestLeftPair, closestRightPair]
	      	where closestLeftPair =  dcClosest leftByX
	              closestRightPair = dcClosest rightByX	 	      	        
	      pairsWithinMinimumDelta = sortBy (compare `on` snd) $ filter withinMinimumDelta sortedByX
	        where withinMinimumDelta (x, _) = abs (xMidPoint - x) <= distance' closestPair	
	                where (xMidPoint, _) = last leftByX

distance' :: Floating a => Pair a -> a
distance' (P (x1, y1) (x2, y2)) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)	 


distance :: Floating a => (Point a, Point a) -> a
distance ((x1, y1), (x2, y2)) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)	  

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then (take size ls) : windowed size xs else windowed size xs	


type R a = State StdGen a
rand :: R Double
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

randPair :: R (Double, Double)
randPair = do
  x <- rand
  y <- rand
  return (x,y)

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

normals :: R [(Double, Double)]
normals = mapM (\_ -> randPair) $ repeat ()

main = do 
	args <- getArgs
	let numberOfPairs = read (head args) :: Int
	if length args > 1 && args !! 1 == "bf" then 
		putStrLn $ show ( (bfClosest $ take numberOfPairs $ runRandom normals 42))
	else 
		putStrLn $ show ( (dcClosest $ take numberOfPairs $ runRandom normals 42))



prop_closest_pairs xs = length xs >= 2 ==> dcClosest xs == (fromJust $ bfClosest xs)

prop_groups_adjacent_items n xs = n < length xs ==>  
								  not (null xs) ==>
								  n > 0 ==>
								  	(last $  last $ windowed n xs) == last xs

prop_nothing_if_n_too_large n xs = n > length xs ==>  windowed n xs == []

prop_size_of_sub_arrays_is_n n xs =  n > 0 ==> all (\subArray -> length subArray == n)  (windowed n xs)