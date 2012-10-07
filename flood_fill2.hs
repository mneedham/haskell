module FloodFill2 (floodFill2) where

import Control.Monad (when, unless)
import Data.Array (Array(), Ix(), bounds)
import Data.Array.ST (runSTArray, thaw, inRange, readArray, writeArray)

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

neighbours :: Num i => (i, i) -> [(i, i)]
neighbours (x, y) = [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]