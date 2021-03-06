import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import System
import Data.Set hiding (map)

rabinKarp :: String -> [String] -> Int
rabinKarp text patterns = 
	if length patterns == 0 
		then -1 
	else 
		fromMaybe (-1) $ mapOnMaybe fst $ find matchingString $ zip [0..] $ scanl nextHash (hash text m) $ windowed (m+1) text					
	where n = length text
	      m = length $ head patterns      
	      nextHash currentHash chars = reHash currentHash (head chars) (last chars) m
	      matchingString (offset, textHash) = contains (\hash -> hash == textHash) patternHashes && contains (\pattern -> pattern == subString text offset m) patterns
	      patternHashes = map (\pattern -> hash pattern m) patterns

contains :: (a -> Bool) -> [a] -> Bool
contains fn values = trueOrFalse $ find fn values
	where trueOrFalse Nothing = False 
	      trueOrFalse (Just x) = True

mapOnMaybe :: (a -> b) -> Maybe a -> Maybe b
mapOnMaybe fn (Just x) = Just (fn x)   
mapOnMaybe _ (Nothing) = Nothing  
	      
subString text start end = take end $ drop start text

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then take size ls : windowed size xs else windowed size xs		      

globalQ = 1920475943
globalR = 256

hash = hash' globalR globalQ
hash' r q string m = foldl (\acc x -> (r * acc + ord x) `mod` q) 0 $ take m string

reHash = reHash' globalR globalQ
reHash' r q existingHash firstChar nextChar m = 
	(takeOffFirstChar `mod` fromIntegral q * fromIntegral r + ord nextChar) `mod` fromIntegral q
	where 
		rm = if m >0 then (fromIntegral r ^ fromIntegral (m-1)) `mod` fromIntegral q else 0
		takeOffFirstChar = existingHash - fromIntegral rm * ord firstChar

hash3 :: [Char] -> Int -> Int
hash3 = hash3' globalR  globalQ	
hash3' r q string m = fromIntegral $ (flip mod q . sum . map (\(pow, char) -> fromIntegral (ord char) * fromIntegral (r ^ pow)) . zip [m-1, m-2..0]) string

hash2 :: [Char] -> Int -> Int
hash2 = hash2' globalR  globalQ	
hash2' r q string m = (flip mod q . sum . map (\(pow, char) -> ord char *  (r ^ pow)) . zip [m-1, m-2..0]) string

hash4 :: [Char] -> Int -> Int
hash4 value m = fromInteger $ hash4' (toInteger globalR) (toInteger globalQ) value m	
hash4' :: Integer -> Integer -> [Char] -> Int -> Integer
hash4' r q string m = (sum $ map (\(pow, char) -> toInteger (ord char) *  toInteger (r ^ pow)) $ zip [m-1, m-2..0] string) `mod` q

hash5' r q value m = fromInteger (sum $ map (\(pow, char) -> toInteger (ord char) * toInteger (r ^ pow)) $ zip [m-1, m-2..0] value) `mod` (toInteger q)

getLines = (liftM lines . readFile)

greenify text = "\x1b[32m" ++ text ++ "\x1b[0m"
flatten = foldl (++) ""

main = do 		
		args <- getArgs		
		if length args < 2 then putStrLn "Usage: ./rabinKarp pattern file"
		else
			do				
				let (file:patterns) = args
				    m = length $ head patterns
				lines <- getLines file
				putStrLn  $ fromMaybe noMatch . mapOnMaybe (format m) . find aMatch . map (\(idx, line) -> (idx, line, rabinKarp line patterns)) . (zip [1..]) $ lines		
				where format m (idx, line, match) = flatten [show idx, " ", take match line, greenify (subString line match m), drop (match + m) line]
				      aMatch (_,_, match) = match /= -1
				      noMatch = "No match found"
				      

--reHash = reHash' globalR globalQ
--reHash' r q existingHash firstChar nextChar m = 
--	(takeOffFirstChar `mod` fromIntegral q) * fromIntegral r + ord nextChar `mod` fromIntegral q
--	where 
--		rm = if m >0 then fromIntegral r ^ fromIntegral (m-1) `mod` fromIntegral q else 0
--		takeOffFirstChar = existingHash - fromIntegral rm * ord firstChar


--(256 ^ 7 * ord 'm') + (256 ^ 6 * ord 'a') + (256 ^ 5 * ord 'r') + (256 ^ 4 * ord 'k') + (256 ^ 3 * ord 'u') + (256 ^ 2 * ord 's') + (256 ^ 1 * ord 'a') + (256 ^ 0 * ord 'u')		

--320570939