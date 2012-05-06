import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Control.Applicative ((<$>))
import Data.Maybe
import System
import Data.List
import Data.List.Split
import Data.Function

bfClosest :: (Ord a, Floating a) => [(a, a)] -> Maybe ((a, a), (a, a))
bfClosest pairs = 
	snd $ foldl (\ (min, soFar) (p1, p2) -> if distance p1 p2 < min then (distance p1 p2, Just(p1, p2)) else (min, soFar)) 
		  (2^64, Nothing) 
		  [(pairs !! i, pairs !! j) | i <- [0..length pairs - 1], j <- [0..length pairs-1 ], i /= j]

--dcClosest :: (Ord a, Floating a) => [(a, a)] -> Maybe ((a, a), (a, a))
--dcClosest pairs = 
--	let (leftByX:rightByX) = chunk (length sortedByX `div` 2) sortedByX
--	    leftResult = dcClosest(leftByX)
--	    rightResult = dcClosest(rightByX)
--		(leftp1,leftp2) = fromJust leftResult
--		(rightp1,rightp2) = fromJust rightResult
--		result = if distance leftp1 leftp2 < distance rightp1 rightp2 then (leftp1, leftp2) else (rightp1, rightp2)
--	    midX = fst $ last leftByX
--	    bandwidth = distance (fst result) (snd result)
--	    inBandByX = filter (\p -> abs (midX - fst p) <= bandwidth) sortedByX
--	    inBandByY = sortBy (compare `on` snd) inBandByX
	    
--	where sortedByX = sortBy compare pairs

distance (x1, y1) (x2, y2) =  sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

--scanl (\(close, dist) p ->  (close,dist)) (Just((0,0), (5,5)), distance (0,0) (5,5)) blah

x = scanl (\(close, dist) p -> 
		let pDistance = distance (p !! 1) (p !! 0) in
		if pDistance <  distance (fst $ fromJust close) (snd $ fromJust close) 
		then (Just(p!!0, p!!1), pDistance) 
		else  (close,dist)) 
	  (Just((0,0), (5,5)), 7.0710678118654755) 
	  [[(0,0),(1,1)],[(1,1),(2,2)], [(1.1,1.1),(1.2,2)], [(1.7,1.1),(1.2,2)]]

--(distance (p !! 1) (p !! 0)) <  (distance (fst close) (snd close))




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
	putStrLn $ show (fromJust (bfClosest $ take numberOfPairs $ runRandom normals 42))
