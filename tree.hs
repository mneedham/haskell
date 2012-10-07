import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative
import Control.Monad
--data Node =  Leaf Int | Branch [Node]

--valueOf :: Node -> Int
--valueOf (Leaf x) = x

--getLeafValues :: Node -> [Int]
--getLeafValues (Leaf x) = [x]
--getLeafValues (Branch nodes) = foldl (\nodesSoFar node -> nodesSoFar ++ (getLeafValues node)) [] nodes   

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  f x `mappend`  F.foldMap f r 

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )      

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x            

type Birds = Int  
type Pole = (Birds,Birds)  

--landLeft :: Birds -> Pole -> Pole  
--landLeft n (left,right) = (left + n,right)  
  
--landRight :: Birds -> Pole -> Pole  
--landRight n (left,right) = (left,right + n)  

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x  
    
justTest :: Maybe [Char] -> Maybe Char
justTest Nothing = Nothing
justTest (Just (x:xs)) = return x
justTest (Just []) = Nothing

wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  

type KnightPos = (Int,Int)     

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r') 


moveKnight2 :: KnightPos -> [KnightPos]
moveKnight2 (c,r) = nextPositions  >>= \(c', r') -> guard (c' `elem` [1..8] && r' `elem` [1..8]) >> return (c', r')       
	where nextPositions = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]


canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  

canReachInX :: KnightPos -> KnightPos -> Int -> Bool  
canReachInX start end x = end `elem` (foldM (\acc x -> return acc >>= moveKnight) start(replicate 3 "x"))

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight