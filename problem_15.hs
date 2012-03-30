import Data.Array

reached_destination :: (Int, Int) -> (Int, Int) -> Bool
reached_destination (origin_x, origin_y) (dest_x, dest_y)  = origin_x == dest_x && origin_y == dest_y 

routes :: Int -> Int
routes size =
	arr ! (size, size)
	where
		arr = array ((0,0),(size,size)) [((x,y), inner (x,y) size) | x<-[0..size], y<-[0..size]]
		inner origin@(x, y) size 
			| x == 0 && y == 0 = 0
			| x == 0 || y == 0 = 1
			| otherwise = arr ! (x-1, y) + arr ! (x, y-1)

memoize_grid size = array ((0,0),(size,size)) [((x,y),Nothing) | x<-[0..size], y<-[0..size]] 

a1 = array (0, 5) [(x, Nothing) | x <- [0..5]]

b = array ((0,0),(19,19)) [((x,y),Nothing) | x<-[0..19], y<-[0..19]] 