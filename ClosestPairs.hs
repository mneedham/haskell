import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Data.Maybe
import System
import Data.List
import Data.List.Split
import Data.Function

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
		fst $ last $ scanl (\(closest, shouldExit) (p1,p2) -> 
							  let currentClosest = minimumBy (compare `on` distance') [closest, P p1 p2] in
							  if abs (yDelta p1 p2)  >= (distance' closestPair) then (closest, True)
							  else (currentClosest, False))
	          (closestPair, False)
	          (combos pairsWithinMinimumDelta)
	where sortedByX = sortBy compare pairs	      
	      (leftByX:rightByX:_) = [take (length sortedByX `div` 2) sortedByX, drop (length sortedByX `div` 2) sortedByX]      
	      closestPair = minimumBy (compare `on` distance') [closestLeftPair, closestRightPair]
	      	where closestLeftPair =  dcClosest leftByX
	              closestRightPair = dcClosest rightByX	 	      	        
	      pairsWithinMinimumDelta = sortBy (compare `on` snd) $ filter withinMinimumDelta sortedByX
	        where withinMinimumDelta (x, _) = abs (xMidPoint - x) <= distance' closestPair	
	                where (xMidPoint, _) = last leftByX

yDelta :: Floating a => Point a -> Point a -> a
yDelta (x1, y1) (x2, y2) = y2 - y1

distance' :: Floating a => Pair a -> a
distance' (P (x1, y1) (x2, y2)) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)	 

distance'' :: Floating a => (a, a) -> (a, a) -> a
distance'' (x1, y1) (x2, y2) = sqrt (dx*dx + dy*dy)
  where
    dx = x1-x2
    dy = y1-y2


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