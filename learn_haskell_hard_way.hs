import Data.List
import Data.Maybe
import System

evenSum :: [Integer] -> Integer

evenSum = accumSum 0
	where
		accumSum n [] = n 
		accumSum n (x:xs) = if even x then accumSum (n+x) xs else accumSum n xs


evenSum2 l = mysum 0 (filter even l)
    where 
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs
      
type Name   = String
type Color  = String

showInfos :: Name ->  Color -> String
showInfos name color =  "Name: " ++ name
                        ++ ", Color: " ++ color       		

data BinTree a = Empty 
                 | Node a (BinTree a) (BinTree a)       
                              
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))                                               

-- declare BinTree a to be an instance of Show
instance (Show a) => Show (BinTree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree 
    --   show a tree and start each line with pref
    -- We don't display Empty tree
    treeshow pref Empty = ""
    -- Leaf
    treeshow pref (Node x Empty Empty) = 
                  (pshow pref x)

    -- Right branch is empty
    treeshow pref (Node x left Empty) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    -- Left branch is empty
    treeshow pref (Node x Empty right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- Tree with left and right sons non empty
    treeshow pref (Node x left right) = 
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- show a tree using some prefixes to make it nice
    showSon pref before next t = 
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replace "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- replace on char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x 
              | x == c = new
              | otherwise = x:[] -- "x"                             

toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"                  

askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by comma):"
  input <- getLine
  maybeList $ getListFromString input 
  where maybeList (Just l) = return l
        maybeList Nothing = askUser

nickname :: String -> Maybe (String, String)
nickname name = find (\n -> fst n == name)  [("Mark", "Junior"), ("Will", "Dingo"), ("Paul", "Dangle"), ("Mum", "Mowgli"), ("Dad", "JB"), ("Deepali", "Chamak"), ("Zaynab", "Mama Nab")]

askUser2 :: IO (String, String)
askUser2 = putStrLn "Whose nickname do you want to know?" >>
           getLine >>= \name ->
           maybeName name (nickname name)
           where maybeName _ (Just n) = return n
                 maybeName name Nothing = retryAsk name        

retryAsk :: String -> IO (String, String)
retryAsk name = putStrLn("Don't know the nickname of " ++ name) >>
                askUser2   

--main :: IO ()
--main = do
--  putStrLn "Enter a list of numbers (separated by comma):"
--  input <- getLine
--  maybeList $ getListFromString input 
--  where
--  	maybeList (Just l) = print (sum l)
--  	maybeList Nothing = error "Bad format. Good Bye."           

--main :: IO ()
--main = do
--  list <- askUser
--  print $ sum list

--foldr (++) [] $ map charToArray ["1", "2"]

charToArray :: [Char] -> [Int]
charToArray [] = []
charToArray (x:xs) = (read [x]::Int) : (charToArray xs)		

intify :: [String] -> [Maybe Int]
intify =  map maybeRead2


maybeRead2 :: String -> Maybe Int
maybeRead2 = fmap fst . listToMaybe . reads

--main = do args <- getArgs
--          print (sum $ map (fromMaybe 0)  $ (intify args))

main = askUser2 >>= \(name, nickname) -> 
  print(name ++ "'s nickname is: " ++ nickname)

a :: [(Int, String)]
a = reads "1"
