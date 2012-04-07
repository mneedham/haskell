module PrintArray (printGrid, grid, toComplexArray, toSimpleArray, textRepresentation) where

import Data.Array
import Data.List

grid :: Int -> a -> Array(Int, Int) a
grid size value = array ((0,0),(size-1,size-1)) [((x,y),value) | x<-[0..size-1], y<-[0..size-1]]

printGrid :: Show a => Array (Int, Int) a ->  IO [()]
printGrid =  mapM (putStrLn . textRepresentation) . toSimpleArray

toSimpleArray :: Array (Int, Int) a -> [[a]]	
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]] 
	where ((lowx, lowy), (highx, highy)) =  bounds grid

toComplexArray :: [[a]] -> Array(Int, Int) a
toComplexArray grid = array ((0,0),((length $ grid !! 0) - 1,(length grid) - 1))  entries  
	where entries = concatMap (\z -> map (\y -> ((fst y, fst z), snd y))  (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) grid

textRepresentation :: Show a => [a] -> String
textRepresentation =  intercalate " " . map show