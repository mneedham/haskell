import Data.Array
import PrintArray
import Control.Monad
import Control.Monad.State

import Control.Monad (when, unless)
import Data.Array (Array(), Ix(), bounds)
import Data.Array.ST (runSTArray, thaw, inRange, readArray, writeArray)

import Prelude hiding (and) 

data Colour = White | Black | Blue | Green | Red deriving (Show, Eq)
 
inBounds :: Array (Int, Int) Colour -> (Int, Int) -> Bool
inBounds grid (x, y) = x >= lowx && x <= highx && y >= lowy && y <= highy
	where ((lowx, lowy), (highx, highy)) =  bounds grid

	
replace :: Array (Int, Int) Colour -> (Int, Int) -> Colour -> Array (Int, Int) Colour
replace grid point replacement = if inBounds grid point then grid // [(point, replacement)] else grid	

floodFill :: Array (Int, Int) Colour ->  (Int, Int) -> Colour -> Colour -> Array (Int, Int) Colour
floodFill grid point@(x, y) target replacement = 	
	if((not $ inBounds grid point) ||  grid ! (x,y) /= target || target == replacement) then grid 
	else 
		gridNorth
		where grid' = replace grid point replacement
		      gridEast = floodFill grid' (x+1, y) target replacement
		      gridWest = floodFill gridEast (x-1, y) target replacement
		      gridSouth = floodFill gridWest (x, y+1) target replacement
		      gridNorth = floodFill gridSouth (x, y-1) target replacement

floodFill3 :: Array (Int, Int) Colour ->  (Int, Int) -> Colour -> Colour -> Array (Int, Int) Colour 
floodFill3 grid point target replacement = 
	if(target == replacement) then
		grid
	else	
	    let replacedSquare = if onGrid point then grid // [(point, replacement)] else grid 
	        validNeighbours = filter (onGrid `and` sameAsTarget) neighbours in	

		foldl (\grid point -> floodFill3 grid point target replacement) replacedSquare validNeighbours 
		
		where neighbours = let (x,y) = point in [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
		      sameAsTarget point = grid ! point == target	
		      onGrid = inRange $ bounds grid     			      

neighbours :: Num i => (i, i) -> [(i, i)]
neighbours (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both f g x = f x && g x

state_floodFill :: Array (Int, Int) Colour ->  (Int, Int) -> Colour -> Colour -> Array (Int, Int) Colour
state_floodFill grid point target replacement = evalState (state_floodFill' point target replacement) grid
state_floodFill' point@(x,y) target replacement = do 
	grid <- get	
	if(target == replacement) then 
		return grid 
	else 
		let neighbouring = filter (both (onGrid grid) (sameAsTarget grid)) (neighbours point) in
		foldM (\acc val -> return $ state_floodFill acc val target replacement) (replace grid point replacement) neighbouring
		where sameAsTarget grid point = grid ! point == target	
		      onGrid grid = inRange $ bounds grid

first  lst = first' lst []
first' [] _ = []
first' (h:t) found = 
       if h `elem` found then False:(first' t found)
       else True:(first' t (h:found))

state_first lst = evalState (state_first' lst) []
state_first' []    = return []
state_first' (h:t) = do state <- get
                        let found = h `elem` state
                        put (if found then state else (h:state))
                        rest <- state_first' t
                        return $ (not found):rest	       


floodFill2 :: (Num i, Ix i, Eq c) => Array (i, i) c ->  (i, i) -> c -> c -> Array (i, i) c
floodFill2 grid point target replacement
  = if target == replacement
    then grid
    else runSTArray $ do
      mutableGrid <- thaw grid
      doFloodFill mutableGrid
      return mutableGrid

  where
    outOfBounds = not . inRange (bounds grid)
    
    doFloodFill mutableGrid = go point
      where
        go p = 
          unless (outOfBounds p) $ do
            currentColour <- readArray mutableGrid p
            when (currentColour == target) $ do
              writeArray mutableGrid p replacement
              mapM_ go $ neighbours p


data FibState = F {previous, current :: Integer}
fibState0 = F {previous = 1, current = 0}

currentFib :: State FibState Integer
currentFib = gets current

nextFib :: State FibState Integer
nextFib = do
    F p c <- get
    let n = p+c
    put (F c n)
    return n

getNFibs :: Int -> State FibState [Integer]
getNFibs k = replicateM k nextFib

--main :: IO ()
--main = print $ evalState (liftM2 (:) currentFib (getNFibs 5) ) fibState0


listToArray :: [a] -> Array Int a
listToArray l = listArray (0, length l - 1) l

testGrid = [[White, White, White], [Black, White, White], [White, Blue, Blue]]