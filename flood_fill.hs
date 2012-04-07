import Data.Array
import PrintArray

data Colour = White | Black | Blue | Green | Red deriving (Show, Eq)
 
inBounds :: Array (Int, Int) Colour -> (Int, Int) -> Bool
inBounds grid (x, y) = x >= lowx && x <= highx && y >= lowy && y <= highy
	where ((lowx, lowy), (highx, highy)) =  bounds grid

replace :: Array (Int, Int) Colour -> (Int, Int) -> Colour -> Array (Int, Int) Colour
replace grid point@(x, y) replacement = if inBounds grid point then grid // [((x, y), replacement)] else grid	

floodFill :: Array (Int, Int) Colour ->  (Int, Int) -> Colour -> Colour -> Array (Int, Int) Colour
floodFill grid point@(x, y) target replacement = 	
	if((not $ inBounds grid point) ||  grid ! (x,y) /= target) then grid 
	else 
		gridNorth
		where grid' = replace grid point replacement
		      gridEast = floodFill grid' (x+1, y) target replacement
		      gridWest = floodFill gridEast (x-1, y) target replacement
		      gridSouth = floodFill gridWest (x, y+1) target replacement
		      gridNorth = floodFill gridSouth (x, y-1) target replacement

testGrid = [[White, White, White], [Black, White, White], [White, Blue, Blue]]