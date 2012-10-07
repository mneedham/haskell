import Data.Tuple
import Data.Maybe
import Data.Array
import Debug.Trace

data UKCoin = OnePence | TwoPence | FivePence | TenPence | TwentyPence | FiftyPence | OnePound | TwoPounds 
              deriving (Eq, Show) 

data USCoin = Quarter | Dime | Nickel | Penny deriving (Eq, Show)



--class Coin a where 
--	value :: a -> Int

--instance Coin USCoin where value coin = fromEnum coin	
--instance Coin UKCoin where value coin = fromEnum coin	

class Eq a => Coin a where 
	table :: [(a, Int)]

	value :: a -> Int
	value = fromJust . flip lookup table

instance Coin USCoin where 
	table = [(Quarter, 25), (Dime, 10), (Nickel, 5), (Penny, 1)]	

instance Coin UKCoin where 
	table = [(OnePence, 1), (TwoPence, 2), (FivePence, 5), (TenPence, 10), (TwentyPence, 20), (FiftyPence, 50), (OnePound, 100), (TwoPounds, 200)]

--data Combination = Combination [USCoin] deriving (Show)

--combinations :: Int -> [USCoin] -> [USCoin] -> [[USCoin]]
--combinations 0 current _ = [current]
--combinations _  current [] = []
--combinations total inProgress coinsToUse@(x:xs) = 	
--	concatMap (\ times ->  combinations (total - (times * coinValue)) (inProgress ++ (take times $ repeat x)) xs)  [maxNumberOfThisCoin, maxNumberOfThisCoin-1..0]
--	where coinValue = fromEnum x 
--	      maxNumberOfThisCoin = (total `div` coinValue)

combinations :: (Coin a) => Int -> [a] -> [a] -> [[a]]
combinations 0 current _ = [current]
combinations _  current [] = []
combinations total p (c:cs) = concatMap (\ times -> combinations (total - (times * value c)) (p ++ replicate times c) cs) 
                                        [0,1..(total `div` value c)] 

--combinationsUK :: Int -> [UKCoin] -> [UKCoin] -> [[UKCoin]]
--combinationsUK 0 current _ = [current]
--combinationsUK _  current [] = []
--combinationsUK total inProgress (c:cs) = 
--    concatMap (\ times -> combinationsUK (total - (times * coinValue)) (inProgress ++ replicate times c) cs) 
--              [0,1..maxNumberOfThisCoin]  
--	where coinValue = fromEnum c
--	      maxNumberOfThisCoin = (total `div` coinValue)

--combinationsUKm :: Int -> [UKCoin] -> [[UKCoin]]
--combinationsUKm total coinsToUse@(x:xs) =
--	arr ! (0, total)
--	where arr = array ((0,0),(0,total)) [((0,y), inner y [] coinsToUse) | y <-[0..total]]
--	      inner 0 current _ = [current]
--	      inner _  current [] = []
--	      inner total inProgress coinsToUse@(x:xs) = concatMap (\ times -> inner (total - (times * coinValue)) (inProgress ++ replicate times x) xs)  [maxNumberOfThisCoin,maxNumberOfThisCoin-1,0]
--	      	where coinValue = fromEnum x 	      		  
--	      	      maxNumberOfThisCoin = (total `div` coinValue)


sums :: Int -> [Int] -> [(Int, Int)] -> [[(Int, Int)]]

sums 0 remaining_coins current_way = [current_way]
sums p [] current_way = []
sums p (c:cs) current_way = concatMap (\x -> sums (p - x*c) cs ((x,c):current_way)) [0, 1 .. p `div` c]

combinations2 0 _ = 1
combinations2 n [] = 1
combinations2 n ccs@(c:cs) = if n < 0 then 0 else combinations2 (n-c) ccs + combinations2 n cs

