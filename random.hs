
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

factors x (p:ps) = 
	if p*p > x then [x]
	else if x `mod` p == 0 then p : factors (x `div` p) (p:ps)
    else factors x ps

isPrime x = if (length (factors x myPrimes) == 1) then True else False  

myPrimes = 2 : [x | x <- [3,5..], isPrime x]   

greatestPrimeFactor x = last (factors x myPrimes)      

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim


--recscanl2 :: (a -> b -> a) -> a -> [b] -> [a]
--recscanl2 fn acc xs = recscanin fn [acc] xs 
--	where 
--		recscanin :: ([a] -> b -> [a]) -> [a] -> [b] -> [a]
--		recscanin f acc2 [] = acc2
--		recscanin f acc2 (x:xs) = [fn (last acc) x] ++ (recscanin fn ([fn (last acc) x] ++ acc) xs)

-- put the initial seed value 
-- then add it to the current value and return that 

recscanl :: (a -> b -> a) -> a -> [b] -> [a]
recscanl fn acc [] = []
recscanl fn acc (x:xs) = 
  [fn acc x] ++ (recscanl fn (fn acc x) xs)

recscanl2                   :: (a -> b -> a) -> a -> [b] -> [a]
recscanl2 f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> recscanl2 f (f q x) xs) 

recscanl3 :: (a -> b -> a) -> a -> [b] -> [a]
recscanl3 fn acc ls = acc : (case ls of
            [] -> []
            x:xs -> recscanl3 fn (fn acc x) xs)
            

foldscanl :: (a -> b -> a) -> a -> [b] -> [a]
foldscanl fn acc ls = foldl (\ x y -> x ++ [fn (last x) y]) [acc] ls

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "no maximum for an empty list"
maximum' [x] = x
maximum' (x:xs) 
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs