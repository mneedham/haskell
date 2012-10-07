 import Data.Monoid
 import Data.Foldable
 import Control.Monad.Writer
 import Control.Monad.State
 import Data.List

 data MaxSoFar a = MaxSoFar { getMaxSoFar :: a } deriving (Eq, Ord, Read, Show, Bounded)
 instance (Num a,  Ord a, Bounded a) => Monoid (MaxSoFar a) where
 	mempty = MaxSoFar (minBound)
 	MaxSoFar x `mappend` MaxSoFar y = MaxSoFar (max x y)

 data MinSoFar a = MinSoFar { getMinSoFar :: a } deriving (Eq, Ord, Read, Show, Bounded)
 instance (Num a,  Ord a, Bounded a) => Monoid (MinSoFar a) where
 	mempty = MinSoFar maxBound
 	MinSoFar x `mappend` MinSoFar y = MinSoFar (min x y) 	

--maxSoFar = foldMap MaxSoFar [1..20]	
--foldMap (\x -> (MaxSoFar x, MinSoFar x)) ([1..20] :: [Int])

 data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

 isBigGang :: Int -> (Bool, String)  
 isBigGang x = (x > 9, "Compared gang size to 9.") 

 instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r 

 --zwl n = do
	--		drop n     -- Drop the words from the word stream before we
	--		w <- head  -- get the desired n-th word
	--		liftIO $ putStrLn $ "Got the word: " ++ w
 --                       -- From the line stream, drop the lines that
 --                       -- do not contain the word w
	--		lift $ dropWhile (not . isInfixOf w)
 --                       -- Read the first line that does contain w
	--		l <- lift $ head
	--		return (l,w)   


   


 --fact1 :: Integer -> Writer String Integer
 --fact1 0 = return 1
 --fact1 n = do
 --  let n' = n-1
 --  tell $ "We've taken one away from " ++ show n ++ "\n"
 --  m <- fact1 n'
 --  tell $ "We've called f " ++ show m ++ "\n"
 --  let r = n*m
 --  tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
 --  return r


--newtype MaxSoFar a = MaxSoFar { getMaxSoFar :: a } deriving (Eq, Ord, Read, Show, Bounded)   

--instance Num a => Monoid (MaxSoFar a) where
--        mempty = MaxSoFar (minBound)
--        MaxSoFar x `mappend` MaxSoFar y = MaxSoFar (max [x, y])

