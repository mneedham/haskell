import Data.Array

routes_through_grid :: Int -> [[(Int, Int)]]
routes_through_grid size = [[(1,2)], [(1,2)]]

--route_from :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
--route_from origin@(origin_x, origin_y) dest@(dest_x, dest_y) = 
--	if(reached_destination origin dest) then [(dest_x, dest_y)]
--	else if(origin_x == dest_x) then
--		(origin_x, origin_y) : route_from (origin_x, origin_y+1) (dest_x, dest_y)
--	else if(origin_y == dest_y) then
--		(origin_x, origin_y) : route_from (origin_x+1, origin_y) (dest_x, dest_y)
--	else 
--		(origin_x, origin_y) : route_from (origin_x, origin_y+1) (dest_x, dest_y) ++ route_from (origin_x+1, origin_y+1) (dest_x, dest_y)	 

reached_destination :: (Int, Int) -> (Int, Int) -> Bool
reached_destination (origin_x, origin_y) (dest_x, dest_y)  = origin_x == dest_x && origin_y == dest_y 

routes :: (Int, Int) -> Int -> Int
routes origin size =
	arr ! (size, size)
	where
		arr = array ((0,0),(size,size)) [((x,y),inner (x,y) size) | x<-[0..size], y<-[0..size]]
		inner origin@(x, y) size 
			| x == 0 && y == 0 = 0
			| x == 0 || y == 0 = 1
			| otherwise = arr ! (x-1, y) + arr ! (x, y-1)

--memoize_grid :: Int -> [[Maybe Int]]
--memoize_grid size = [[Nothing | x <- [1..20]] | y <- [1..size]]


memoize_grid size = array ((0,0),(size,size)) [((x,y),Nothing) | x<-[0..size], y<-[0..size]] 



a1 = array (0, 5) [(x, Nothing) | x <- [0..5]]

b = array ((0,0),(19,19)) [((x,y),Nothing) | x<-[0..19], y<-[0..19]] 