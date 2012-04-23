import Data.HashTable
import Data.Char


dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli2 xs = do 
	x <- xs
	[x, x]

repli xs n = do
	x <- xs
	[x | y <- [1..n]]
	
repli2 xs n = concatMap (\x -> [x | y <- [1..n]]) xs	 	

dropEvery xs n = map fst $ filter (\(x, idx) -> mod idx n /= 0)  $ zip xs [1..]


rabinKarp text pattern =
	let n = length text
	    m = length pattern 
	    textHash = hashString (take m text) m 
	    patternHash = hashString pattern m in
		
		if length (take m text) == 0 then "fail"
		else
			if textHash == patternHash then "yay"
			else rabinKarp (drop 1 text) pattern 	
	where 
		hashString value length = sum $ map ord $ take length value

rabinKarp2 :: String -> String -> Int
rabinKarp2 text pattern =
	if length matchingPositions == 0 then -1 else matchingPositions !! 0 
	where 
		matchingPositions = map fst $ filter ((==True) . snd) $  zip [0..] checkPositions
		checkPositions = [textHash == patternHash && subString == pattern | i <- [0..n-m], 
							let subString = subStringText i m,
							let textHash = hashString subString m, 
							let patternHash = hashString pattern m
						 ]	
		n = length text
		m = length pattern
		subStringText start end = (take end $ (drop start) text)
		hashString value length = sum $ map ord $ take length value

--rabinKarp3 :: String -> String -> Int
--rabinKarp3 text pattern =
--	if length matchingPositions == 0 then -1 else matchingPositions !! 0 
--	where 
--		matchingPositions = karpInner (concat $ map myOrd (take m text)) (concat $ map myOrd (take 1 (drop m text))) 0  
--		karpInner hash next i = 
--			if (n-m > i) then
--				if hash == patternHash && subStringText i m == pattern 
--				then [i] ++ karpInner (newHash hash next) (concat $ map myOrd (take 1 (drop (m+i) text)))  (i+1)  	
--				else karpInner (newHash hash next) (concat $ map myOrd (take 1 (drop (m+i) text))) (i+1)
--			else 
--				[]
--		n = length text
--		m = length pattern
--		patternHash = hashString pattern m
--		subStringText start end = (take end $ (drop start) text)
--		hashString value length = sum $ map ord $ take length value		

newHash existingHash nextChar =  
	concat $ reverse $ map (padWithZeros 3 . reverse) $ groups 3 $ reverse $ show $ nextValueCalculation
	where 
		nextValueCalculation = 1000 * (intify existingHash - (1000^(m-1) * intify firstPartOfHash)) + intify nextChar
		m = (length existingHash) `div` 3	
		firstPartOfHash = (take 3 existingHash)
		intify value = read value :: Int

myOrd char = repli "0" (3 - length initial) ++ initial
	where initial = show (ord char)

padWithZeros desiredLength value = repli "0" (desiredLength - length value)	++ value	

groups :: Int -> [a] -> [[a]]
groups size [] = []
groups size ls@(x:xs) = (take size ls) : groups size (drop (size-1) xs)

globalQ = 1920475943
globalR = 256

rabinKarp4 :: String -> String -> [Int]
rabinKarp4 text pattern = 
	if n < m  then [-1]
	else 
		if found initialTextHash text 0 then [0]
		else
			scanl nextHash initialTextHash (windowed (m + 1) text)
			--head $ (map fst $ filter (\(idx, hash) -> found hash text idx) $ zip [0..] $ scanl nextHash initialTextHash (windowed (m + 1) text)) ++ [-1]		 	
	where n = length text
	      m = length pattern
	      initialTextHash = hash text m
	      patternHash = hash pattern m
	      found textHash text offset = (patternHash == textHash) && (pattern == subString text offset m)
	      nextHash currentHash chars = reHash4 currentHash (head chars) (last chars) m
	      
subString text start end = (take end $ (drop start) text)

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then (take size ls) : windowed size xs else windowed size xs	

hash = hash' 256 globalQ
hash' r q string m = foldl (\acc x -> (r * acc + ord x) `mod` q) 0 $ take m string

hash2 = hash' globalR globalQ
hash2' r q string m = (flip mod q . sum . map hashForChar . zip descendingPowersOfM) string
	where descendingPowersOfM = [r ^ pow | pow <- [m-1, m-2..0]]
	      hashForChar (multiplier, char) = ord char * multiplier

--reHash :: Int -> Char -> Char -> Int -> Int
reHash = reHash' globalR globalQ
reHash' r q existingHash firstChar nextChar m = 
	--((existingHash + q - rm * (ord firstChar)) `mod` q * r + (ord nextChar)) `mod` q
	((existingHash + (fromIntegral q) - (fromIntegral rm) * (ord firstChar)) `mod` (fromIntegral q) * (fromIntegral r) + (ord nextChar)) `mod` (fromIntegral q)
	where rm = (r ^ (m-1)) `mod` q

	--((existingHash + q - rm * (ord firstChar)) `mod` q * r + (ord nextChar)) `mod` q	

reHash3 = reHash3' globalR globalQ
reHash3' r q existingHash m = 
	((existingHash + (fromIntegral q) - (fromIntegral rm) * 109) `mod` (fromIntegral q) * (fromIntegral r) + 101) `mod` (fromIntegral q)
	where rm = (fromIntegral r ^ fromIntegral (m-1)) `mod` fromIntegral q 
	

reHash4 = reHash4' globalR globalQ
reHash4' r q existingHash firstChar nextChar m = 
	((existingHash + (fromIntegral q) - (fromIntegral rm) * (ord firstChar)) `mod` (fromIntegral q) * (fromIntegral r) + (ord nextChar)) `mod` (fromIntegral q)
	where rm = (fromIntegral r ^ fromIntegral (m-1)) `mod` fromIntegral q 	