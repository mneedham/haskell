module PrintArray (printGrid, grid) where

import Data.Array

grid :: Int -> a -> Array(Int, Int) a
grid size value = array ((0,0),(size-1,size-1)) [((x,y),value) | x<-[0..size-1], y<-[0..size-1]]

printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ toSimpleArray grid

toSimpleArray :: Array (Int, Int) a -> [[a]]	
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]] 
	where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y) ++ " ") "" row