module Main
  where

primesTo m = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p,p+2*p..m])

primesToQ m = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..m])

primesToG m = 2 : sieve [3,5..m]  where
    sieve (p:xs) 
       | p*p > m = p : xs
       | True    = p : sieve (xs `minus` [p*p,p*p+2*p..])

--primes = 2 : sieve [3,5..]  where
--    sieve []     = []
--    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..])    
    
primesPE = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps
       | x < q  = x : sieve xs q ps
       | True   =     sieve (xs `minus` [q,q+2*p..]) (head t^2) t
                         where (p:t) = ps    
     
     
                         
primes, nonprimes :: [Integer]
primes    = 2 : 3 : (minus [5, 7 ..] nonprimes) 
nonprimes = foldr1 f [[p*p, p*p+2*p ..] | p <- tail primes]
  where 
    f (x:xt) ys = x : (union xt ys)                          

minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : union  xs  (y:ys)
          EQ -> x : union  xs     ys 
          GT -> y : union (x:xs)  ys
union  xs     ys    = xs ++ ys
    

main :: IO ()
main = do
  print (sum $ primesToQ 2000000)    