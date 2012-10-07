import Data.Dynamic

import Control.Conditional
import Debug.Trace
import Data.Maybe

member a lat | null lat = False
			 | otherwise = head lat == a || member a (tail lat)

member2 a [] = False
member2 a (x:xs) = or [x == a, member2 a xs]


rember a [] = []
rember a (x:xs) = if a == x then xs else x : rember a xs			 

firsts [] = []
firsts (x:xs) = head x : firsts xs

insertR new old [] = []
insertR new old (x:xs) = if x == old then  (x : new : xs) else x : insertR new old xs

insertR2 new old [] = []
insertR2 new old (x:xs) | x == old = old:new:xs
						| otherwise = x:insertR2 new old xs

insertL new old [] = []
insertL new old (x:xs) | x == old = new:old:xs
					   | otherwise = x:insertL new old xs

insertL2 new old [] = []
insertL2 new old lat@(x:xs) | x == old = new:lat
					   | otherwise = x:insertL2 new old xs

subst new old [] = []
subst new old lat@(x:xs) | x == old = new:xs
				         | otherwise = x:subst new old xs		
				         
subst2 new o1 o2 [] = []
subst2 new o1 o2 (x:xs) | x == o1 = new:xs
						| x == o2 = new:xs
						| otherwise = x:subst2 new o1 o2 xs

subst2a new o1 o2 [] = []
subst2a new o1 o2 (x:xs) | or [x == o1, x == o2] = new:xs						
						| otherwise = x:subst2a new o1 o2 xs

multirember a [] = []
multirember a (x:xs) | a == x  = multirember a xs 
					 | otherwise = x : multirember a xs 

multiInsertR new old [] = []
multiInsertR new old (x:xs) | x == old = old:new:multiInsertR new old xs
							| otherwise = x:multiInsertR new old xs

multiInsertL new old [] = []
multiInsertL new old (x:xs) | x == old = new:old:multiInsertL new old xs
							| otherwise = x:multiInsertL new old xs	

multiInsertL2 new old lat = cond [(null lat, []),
								  (head lat == old, new:old:multiInsertL2 new old (tail lat)),
								  (otherwise, (head lat):multiInsertL2 new old (tail lat))]

multSubst new old lat = cond [(null lat, []),
							  (head lat == old, new:multSubst new old (tail lat)),
							  (otherwise, (head lat):multSubst new old (tail lat))]

add1 = (+) 1
sub1 = flip (-) 1
isZero = (==) 0

plus x y = cond [(isZero y, x),
 				 (otherwise, plus (add1 x) (sub1 y))]

plus2 x y = cond [(isZero y, x),
				 (otherwise, add1 $ plus2 x (sub1 y))] 
				 
minus x y = cond [(isZero y, x),
				 (otherwise, sub1 $ minus x (sub1 y))] 

addTup tup = cond [(null tup, 0),
                   (otherwise, plus (head tup) (addTup (tail tup)))]

times m n = cond [(isZero n, 0),
                  (otherwise, plus m $ times m (sub1 n))]

tupPlus tup1 tup2 = cond [(null tup1 && null tup2, []),
                          (otherwise, plus (head tup1) (head tup2):tupPlus (tail tup1) (tail tup2))]

tupPlus' tup1 tup2 | null tup1 && null tup2 = []
                   | otherwise = plus (head tup1) (head tup2):tupPlus (tail tup1) (tail tup2)

tupPlus'' [] [] = []
tupPlus'' tup1@(x1:xs1) tup2@(x2:xs2) = plus x1 x2:tupPlus xs1 xs2

tupPlus2 tup1 tup2 = cond [(null tup1, tup2),
       					   (null tup2, tup1),
       					   (otherwise, plus (head tup1) (head tup2):tupPlus2 (tail tup1) (tail tup2))]

greaterThan n m = cond [(isZero n, False),
						(isZero m, True),
						(otherwise, greaterThan (sub1 n) (sub1 m))]

lessThan n m = cond [(isZero m, False),
					 (isZero n, True),
					 (otherwise, lessThan (sub1 n) (sub1 m))]

equalTo n m = cond [(greaterThan n m, False),
  				    (lessThan n m, False),
  				    (otherwise, True)]	
  				    
powerOf n m = cond [(isZero m, 1),
  				    (otherwise, times n (powerOf n (sub1 m)))]	

divide n m = cond [(lessThan n m, 0),
                   (otherwise, add1 $ divide (n-m) m)]
                   
length' lat = cond [(null lat, 0),
                    (otherwise, add1 $ length' (tail lat))]

pick n lat = cond [(isZero (sub1 n), head lat),
                   (otherwise, pick (sub1 n) (tail lat))]
                   
rempick n lat = cond [(isZero (sub1 n), tail lat),
                      (otherwise, head lat : rempick (sub1 n) (tail lat))]

rempick2 n lat = cond [(isOne n, tail lat),
                  (otherwise, head lat : rempick2 (sub1 n) (tail lat))]

occur n lat = cond [(null lat, 0),
                    (equalTo n (head lat), add1 $ occur n (tail lat)),
                    (otherwise, occur n (tail lat))]                  

--data ShowBox = forall s. Show s => SB s
 
--heteroList :: [ShowBox]
--heteroList = [SB (), SB 5, SB True]                      

--instance Show ShowBox where
--  show (SB s) = show s  

--isNumber :: ShowBox -> Bool
--isNumber SB(Int) = True

 --noNums lat :: [ShowBox] -> [ShowBox]
 --noNums lat = cond [(null lat, []),
 --                   ()]

justInt :: Dynamic -> Maybe Int
justInt dyn = fromDynamic dyn :: Maybe Int

isNumber :: Dynamic -> Bool
isNumber x = isJust $ justInt x

toString :: Dynamic -> Maybe String
toString dyn = fromDynamic dyn

lat = [toDyn (5 :: Int), toDyn "pears", toDyn (6 :: Int), toDyn "prunes", toDyn (9 :: Int), toDyn "dates"]

noNums lat = cond [(null lat, []), 
				   (isNumber $ head lat, noNums $ tail lat),
				   (otherwise, head lat : noNums (tail lat))]

allNums lat = cond [(null lat, []), 
				    (not $ isNumber $ head lat, allNums $ tail lat),
				    (otherwise, head lat : allNums (tail lat))]			   
 
isOne n = equalTo n 1 

