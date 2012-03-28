module Main
  where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

main :: IO ()
main = do
  print (primes !! 10000)