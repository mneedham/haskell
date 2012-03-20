import System.IO     
    
problem_1_file = "/Users/mneedham/Documents/algo-class/IntegerArray.txt"

main = do     
    withFile problem_1_file ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents) 