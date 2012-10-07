class Munk a where
	munk :: a -> Int

instance Munk Int where
	munk value = 3 * value	

instance Munk Double where
	munk value = floor (5 * value)		

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 

newtype Pair b a = Pair { getPair :: (a,b) }  

instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  