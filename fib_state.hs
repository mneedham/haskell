module FibState where

import Control.Monad.State
import Control.Monad

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

main :: IO ()
main = print $ evalState (liftM2 (:) currentFib (getNFibs 5) ) fibState0