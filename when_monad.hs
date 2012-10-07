import Control.Monad
import System
import System.Directory

--main = do
--	c <- getChar
--	when (c /= ' ') $ do
--		putChar c
--		main

getFileName :: IO String
getFileName = do
	file <- getLine
	fileExists <- doesFileExist file
	if fileExists then do
		return file				
	else do
		putStrLn $ "File " ++ file ++ " does not exist. Please enter another file"
		getFileName



main = do
	args <- getArgs
	if length args == 0 then do
			putStrLn "No file supplied, sort it out"
			getFileName
	else do
		fileExists <- doesFileExist $ head args
		if fileExists then do
			return "mark"
		else 
			getFileName
				
