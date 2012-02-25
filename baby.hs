data CarParkState = Available Bool Int Int | AlmostFull Bool Int Int | Full Bool Int deriving (Show)
data Action = Entering | Leaving deriving (Show) 
data Sticker = Handicap | None deriving (Show) 

convert instruction 
    | instruction == "EN" = (Entering, None)
    | instruction == "EH" = (Entering, Handicap)
    | instruction == "LN" = (Leaving, None) 
    | instruction == "LH" = (Leaving, Handicap)   
    | otherwise = error "Unrecognised instruction" 

none :: CarParkState -> (Int -> Int) -> CarParkState
none (Available _  normal handicap) fn = Available False (fn normal) handicap
none (AlmostFull _  normal handicap) fn = AlmostFull False (fn normal) handicap

handicap :: CarParkState -> (Int -> Int) -> CarParkState
handicap (Available _  normal handicap) fn = Available False normal (fn handicap)
handicap (AlmostFull _  normal handicap) fn = AlmostFull False normal (fn handicap)

car_park_state :: CarParkState -> (Action, Sticker) -> CarParkState
car_park_state (Available _ 8 handicap) (Entering, None) = AlmostFull True 9 handicap
car_park_state (AlmostFull _ 11 handicap) (Entering, None) = Full True handicap
car_park_state (AlmostFull _ 9 handicap) (Leaving, None) = Available True 8 handicap
car_park_state (Full _ handicap) (Leaving, None) = AlmostFull True 11 handicap
car_park_state (Full _ handicap) (Leaving, Handicap) = Full True handicap
car_park_state (Full _ _) _ = error "The car park is already full!"

car_park_state state (Entering, None) = none state (+ 1)
car_park_state state (Entering, Handicap) = handicap state (+ 1)
car_park_state state (Leaving, None) = none state (subtract 1)
car_park_state state (Leaving, Handicap) = handicap state (subtract 1)

sign_changed :: CarParkState -> Bool
sign_changed (Available True _ _)  = True
sign_changed (AlmostFull True _ _ )= True
sign_changed (Full True _ )= True
sign_changed x = False

instructions = "EN EN EH EN EN EH EN EN LN EN EN LH EN LN LN LN LN"

entry :: [Char] -> [CarParkState]
entry instructions = 
	filter sign_changed (scanl (\acc x -> car_park_state acc x) (Available True 4 2) (map convert (words instructions)))

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "no maximum for an empty list"
maximum' [x] = x
maximum' (x:xs) 
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs


recscanl :: (a -> b -> a) -> a -> [b] -> [a]
recscanl fn acc [] = []
recscanl fn acc (x:xs) = [fn acc x] ++ (recscanl fn (fn acc x) xs)

