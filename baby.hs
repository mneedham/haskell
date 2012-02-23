data CarParkState = Available Bool Int Int | AlmostFull Bool Int Int | Full Bool Int Int deriving (Show)
data Action = Entering | Leaving deriving (Show) 
data Sticker = Handicap | None deriving (Show) 

convert instruction 
    | instruction == "EN" = (Entering, None)
    | instruction == "EH" = (Entering, Handicap)
    | instruction == "LN" = (Leaving, None) 
    | instruction == "LH" = (Leaving, Handicap)   
    | otherwise = error "Unrecognised instruction" 

processed_instructions = map convert (process instructions)    

car_park_state :: CarParkState -> (Action, Sticker) -> CarParkState
car_park_state (Available _ 8 handicap) (Entering, None) = AlmostFull True 9 handicap
car_park_state (AlmostFull _ 11 handicap) (Entering, None) = Full True 12 handicap
car_park_state (AlmostFull _ 9 handicap) (Leaving, None) = Available True 8 handicap
car_park_state (Full _ _ handicap) (Leaving, None) = AlmostFull True 11 handicap

car_park_state (_ _ normal handicap) (Entering, None) = Available False (normal + 1) handicap
car_park_state (Available _ normal handicap) (Entering, Handicap) = Available False normal (handicap + 1)
car_park_state (Available _ normal handicap) (Leaving, None) = Available False (normal - 1) handicap
car_park_state (Available _ normal handicap) (Leaving, Handicap) = Available False normal (handicap - 1)

car_park_state (AlmostFull _ normal handicap) (Entering, None) = AlmostFull False (normal + 1) handicap
car_park_state (AlmostFull _ normal handicap) (Entering, Handicap) = AlmostFull False (normal) (handicap + 1)
car_park_state (AlmostFull _ normal handicap) (Leaving, None) = AlmostFull False (normal - 1) handicap
car_park_state (AlmostFull _ normal handicap) (Leaving, Handicap) = AlmostFull False (normal) (handicap - 1)

car_park_state (Full _ _ _) (_, _) = error "The car park is already full!"

changed :: CarParkState -> Bool
changed (Available True _ _)  = True
changed (AlmostFull True _ _ )= True
changed (Full True _ _ )= True
changed x = False

instructions = "EN EN EH EN EN EH EN EN LN EN EN LH EN LN LN LN LN"
process input = words input

entry :: [Char] -> [CarParkState]
entry instructions = 
	filter changed (scanl (\acc x -> car_park_state acc x) (Available True 4 2) (map convert (process instructions)))