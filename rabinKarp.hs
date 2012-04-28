import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import System

rabinKarp :: String -> String -> Int
rabinKarp text pattern = 
	if pattern == "" 
		then -1 
	else 
		fromMaybe (-1) $ mapOnMaybe fst $ find matchingString $ zip [0..] $ scanl nextHash (hash text m) $ windowed (m+1) text					
	where n = length text
	      m = length pattern	      
	      nextHash currentHash chars = reHash currentHash (head chars) (last chars) m
	      matchingString (offset, textHash) = hash pattern m == textHash && pattern == subString text offset m 

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

hash2 = hash2' globalR globalQ
hash2' r q string m = (flip mod q . sum . map (\(pow, char) -> ord char * (r ^ pow)) . zip [pow | pow <- [m-1, m-2..0]]) string


getLines = (liftM lines . readFile)

greenify text = "\x1b[32m" ++ text ++ "\x1b[0m"
flatten = foldl (++) ""

main = do 		
		args <- getArgs		
		if length args < 2 then putStrLn "Usage: ./rabinKarp pattern file"
		else
			do				
				let (pattern:file:[]) = args
				    m = length pattern				    
				lines <- getLines file
				mapM_ (printFriendly m) . filter matchingLines . map (\(idx, line) -> (idx, line, rabinKarp line pattern)) . (zip [1..]) $ lines		
				where printFriendly m (idx, line, match) = putStrLn (flatten [show idx, " ", take match line, greenify (subString line match m), drop (match + m) line])
				      matchingLines (_,_, match) = match /= -1
				      

--reHash = reHash' globalR globalQ
--reHash' r q existingHash firstChar nextChar m = 
--	(takeOffFirstChar `mod` fromIntegral q) * fromIntegral r + ord nextChar `mod` fromIntegral q
--	where 
--		rm = if m >0 then fromIntegral r ^ fromIntegral (m-1) `mod` fromIntegral q else 0
--		takeOffFirstChar = existingHash - fromIntegral rm * ord firstChar


--(256 ^ 7 * ord 'm') + (256 ^ 6 * ord 'a') + (256 ^ 5 * ord 'r') + (256 ^ 4 * ord 'k') + (256 ^ 3 * ord 'u') + (256 ^ 2 * ord 's') + (256 ^ 1 * ord 'a') + (256 ^ 0 * ord 'u')		

--320570939