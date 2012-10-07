import Data.Maybe
import Data.List
import System
import Control.Monad
import Control.Applicative
import Data.List.Split

translate :: String -> String
translate = map translate_char 

translate_char :: Char -> Char
translate_char char = snd $ fromJust $ find (\x -> fst x == char) alphabet_mappings  

alphabet_mappings = [(' ',' '),('a','y'),('b','h'),('c','e'),('d','s'),('e','o'),('f','c'),('g','v'),('h','x'),('i','d'),('j','u'),('k','i'),('l','g'),('m','l'),('n','b'),('o','k'),('p','r'),('q', 'z'), ('r','t'),('s','n'),('t','w'),('u','j'),('v','p'),('w','f'),('x','m'),('y','a'), ('z', 'q')]

--alphabet_mappings = nub $ foldl (++) [] [zip "ejp mysljylc kd kxveddknmc re jsicpdrysi" "our language is impossible to understand",
--				     zip "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd" "there are twenty six factorial possibilities",
--                     zip "de kr kd eoya kw aej tysr re ujdr lkgc jv" "so it is okay if you want to just give up"]

interactiveFun f = do
       let readInt :: String -> Int
           readInt = read
       x <- fmap readInt getLine
       mapM_ (\x -> putStrLn ("Case #: " ++ x)) . map f =<< replicateM x getLine          

--readInput ::IO
--readInput = do
--       let readInt :: String -> Int
--           readInt = read
--       x <- fmap readInt getLine
--       y <- getLine     
--       return [y]   z

readInput2 :: IO [String]
readInput2 = do
  line <- getLine
  let count :: Int
      count = read line
  lines <- replicateM (count) $ do
    line <- getLine
    return line
  return lines    
  
--readInput3 = do
--  line <- getLine
--  let count :: Int
--      count = read line
--  return $ [do line <- getLine; line | x <- [1..count]]     

main = do
       input <- readInput2
       mapM_ (\(idx, line) -> putStrLn $ "Arg #" ++ show idx ++ " " ++ line ) (zip [1..] input)
       --mapM_ (\(idx, line) -> putStrLn $ "Case #" ++ show idx ++ ": " ++ dancing_with_googlers line) (zip [1..] input)


dancing_with_googlers :: String -> String
dancing_with_googlers line = 
	let (_:surprisingScores:p:googlers) = map intify $ splitOn " " line in
	show $ numberAchievingMinimum surprisingScores p googlers
	where intify x = read x :: Int

bestScoreNoSpecials :: Int -> Int
bestScoreNoSpecials	total = 
	maxValue [[x, y, z] | x <- [1..total],  y <- [1..total],  z <- [1..total], x + y + z == total, (maximum [x, y, z] - minimum [ x,y,z]) <= 1]
	where maxValue [] = 0
	      maxValue xs = maximum $ map maximum xs

bestScoreSpecial :: Int -> Int
bestScoreSpecial	total = 
	maxValue [[x, y, z] | x <- [1..total],  y <- [1..total],  z <- [1..total], x + y + z == total, (maximum [x, y, z] - minimum [ x,y,z]) <= 2]
	where maxValue [] = 0
	      maxValue xs = maximum $ map maximum xs

data Googler = G { totalScore :: Int, bestScoreNoSurprises :: Int, bestScoreSurprises :: Int }

numberAchievingMinimum :: Int -> Int -> [Int] -> Int 
numberAchievingMinimum specials p scores = 
	let googlers = map buildScore scores
	    achievesPWithNoSurpises        = filter (\googler -> bestScoreNoSurprises googler >= p) googlers 
	    doesNotAchievePWithNoSurprises = filter (\googler -> bestScoreNoSurprises googler < p) googlers
	    achievesPWithSurprises = filter (\googler -> bestScoreSurprises googler >= p) doesNotAchievePWithNoSurprises in
    (length achievesPWithNoSurpises) + min specials (length achievesPWithSurprises)
	where buildScore total = G { totalScore = total, bestScoreNoSurprises = bestScoreNoSpecials total, bestScoreSurprises = bestScoreSpecial total }
