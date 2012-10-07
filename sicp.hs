import Data.List
import Debug.Trace
import System

import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, randomR)

ex1_11_rec :: Int -> Int
ex1_11_rec n | n < 3 = n
	         | n >= 3 = ex1_11_rec (n-1) + 2 * ex1_11_rec (n-2) + 3 * ex1_11_rec (n-3)

ex1_11_it :: Int -> Int
ex1_11_it n = ex1_11_inner 2 1 0 n
	where 
		ex1_11_inner f2 f1 f0 n | n == 0 = f0
		ex1_11_inner f2 f1 f0 n | n > 0 = ex1_11_inner (f2 + (2 * f1) + (3 * f0)) f2 f1 (n -1)

pascals :: Int -> [[Int]]
pascals rows = pascalInner rows
	where
		pascalInner rows | rows == 1 = [[1]]
		pascalInner rows | rows == 2 = pascalInner 1 ++ [[1,1]]
		pascalInner rows | rows >= 3 =  previous ++ [1 : (map sum $ windowed 2 (last previous)) ++ [1]]
			where previous = pascalInner (rows-1)

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then (take size ls) : windowed size xs else windowed size xs				


fastExptIt :: Int -> Int -> Int
fastExptIt b n = inner b n 1 where
	inner b n soFar | n == 0 = soFar
	                | n `mod` 2 == 0 = inner (b ^ 2) (n `div` 2) soFar
	                | otherwise = inner b (n - 1) (soFar * b)

mult :: Int -> Int -> Int
mult a b | b == 0 = 0
         | otherwise = a + mult a (b - 1)

double :: Int -> Int
double = (*2)

halve :: Int -> Int
halve n = n `div` 2

square :: Integer -> Integer
square = (^2)

fastMult :: Int -> Int -> Int
fastMult a b | b == 0 =  0
             | b `mod` 2 == 0 = double $ fastMult a (halve b)
             | otherwise = a + fastMult a (b - 1)

fastMultIt :: Int -> Int -> Int
fastMultIt a b = inner a b 0 where
	inner a b soFar | b == 0 = soFar
					| b `mod` 2 == 0 = inner (double a) (halve b) soFar
					| otherwise = inner a (b - 1) (soFar + a)
					
smallestDivisor :: Integer -> Integer
smallestDivisor n = findDivisor n (toInteger 2) where
	findDivisor :: Integer -> Integer -> Integer
	findDivisor n testDivisor | square testDivisor > n = toInteger n 
							  | divides testDivisor n = testDivisor
							  | otherwise = findDivisor n (next testDivisor)   
	divides a b = b `mod` a == 0
	next 2 = 3
	next n = n + 2

isPrime :: Integer -> Bool
isPrime n = smallestDivisor n == n	

expMod :: Integer -> Integer -> Integer -> Integer 
expMod base exp m | exp == 0 = toInteger 1
                  | exp `mod` 2 == 0 = mod (square $ expMod base (exp `div` 2) m) m
                  | otherwise = mod (base * expMod base (exp - 1) m) m

fermatTest :: Integer -> Bool
fermatTest n = tryIt $ random (n-1) where
	tryIt a = expMod a n n == a

random :: Integer -> Integer
random max = fst $ randomR (1,max) (mkStdGen 66)	

isFastPrime :: Int -> Integer -> Bool
isFastPrime times n  | times == 0 = True
					 | fermatTest n = isFastPrime (times - 1) n
					 | otherwise = False

searchForPrimes :: [Integer] -> [Integer]
searchForPrimes range = take 3 $ filter (isFastPrime 20) range     

fastExpt :: Integer -> Integer -> Integer
fastExpt b n | n == 0 = toInteger 1
             | n `mod` 2 == 0 = fastExpt b (n `div` 2) ^ 2
             | otherwise = b * fastExpt b (n - 1)

expMod2 :: Integer -> Integer -> Integer -> Integer
expMod2 base exp m = trace ("called") mod (fastExpt base exp) m

expMod3 :: Integer -> Integer -> Integer -> Integer
expMod3 base exp m | exp == 0 = toInteger 1
				   | exp `mod` 2 == 0 = mod (expMod3 base (exp `div` 2) m * expMod3 base (exp `div` 2) m) m
				   | otherwise = mod (base * expMod3 base (exp - 1) m) m

hardCoreFermatTest :: Integer -> Bool
hardCoreFermatTest n = all tryIt [1..n-1] where 
	tryIt a = expMod a n n == a

inc :: Int -> Int
inc = (+) 1

cube :: Int -> Int
cube = (^3)

sumCubes :: Int -> Int -> Int
sumCubes a b = sicpSum cube a inc b

sumIntegers :: Int -> Int -> Int
sumIntegers a b = sicpSum id a inc b

piSum :: (Ord a1, Fractional a1) => a1 -> a1 -> a1
piSum a b = sicpSum piTerm a piNext b where
	piTerm x = 1 / (x * (x + 2)) 
	piNext x = x + 4

sicpSum :: (Ord a, Num a1) => (a -> a1) -> a -> (a -> a) -> a -> a1
sicpSum term a next b | a > b = 0
                      | otherwise = term a + sicpSum term (next a) next b

--sicpSum2 :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
--sicpSum2 term a next b | a > b = 0
--                       | otherwise = term a + sicpSum2 term (next a) next b                      

--type R a = State StdGen a
--rand :: Integer -> R Integer
--rand max = do
--  gen <- get
--  let (r, gen') = randomR (0, max) gen
--  put gen'
--  return r

--runRandom :: R a -> Int -> a
--runRandom action seed = evalState action $ mkStdGen seed

--normals :: Integer -> R [Integer]
--normals max = mapM (\_ -> rand max) $ repeat ()

--Prelude.take 10 $ runRandom (mapM (\_ -> rand) $ repeat ()) 42

main = do
	args <- getArgs
	let (first:second:_) = map read args in 
		putStrLn $ show $ searchForPrimes ([first, second..])             