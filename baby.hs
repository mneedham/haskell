import Data.Foldable

data CarParkState = Available  { changed :: Bool, normalSpaces :: Int, handicapSpaces :: Int } | 
                    AlmostFull { changed :: Bool, normalSpaces :: Int, handicapSpaces :: Int } | 
                    Full       { changed :: Bool, handicapSpaces :: Int } deriving (Show)
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
handicap (Available _  normal handicap) fn = if handicap_changed handicap fn then Available True normal newHandicap else Available False normal newHandicap where newHandicap = fn handicap
handicap (AlmostFull _  normal handicap) fn = if handicap_changed handicap fn then AlmostFull True normal newHandicap else AlmostFull False normal newHandicap where newHandicap = fn handicap

handicap_changed :: Int -> (Int -> Int) -> Bool
handicap_changed initialHandicap changeFn = changeFn(initialHandicap) == 4 || initialHandicap == 4

car_park_state :: CarParkState -> (Action, Sticker) -> CarParkState
car_park_state state@(Available  {normalSpaces=8})  (Entering, None) = AlmostFull True 9 (handicapSpaces state)
car_park_state state@(AlmostFull {normalSpaces=11}) (Entering, None) = Full True (handicapSpaces state)
car_park_state state@(AlmostFull {normalSpaces=9})  (Leaving, None)  = Available True 8 (handicapSpaces state)

car_park_state state@(Full{}) (Leaving, None) = AlmostFull True 11 (handicapSpaces state)
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
initial_state = (Available True 4 2)
update_state = (\acc x -> car_park_state acc x)

entry :: [Char] -> [CarParkState]
entry = filter sign_changed . scanl update_state initial_state . map convert . words


stringify :: [CarParkState] -> String
stringify states = Prelude.foldl (\acc x -> acc ++ stringifyState x ++ "\n") "" states

singleStringify :: CarParkState -> String
singleStringify (Available _ _ _) = "Parking Available"
singleStringify (AlmostFull _ _ _) = "Parking Almost Full"
singleStringify (Full _ _) = "Parking Full"

handicapStringify :: CarParkState -> Maybe String
handicapStringify (Available _ _ handicap) = if handicap == 4 then Just "No Handicap Parking" else Nothing
handicapStringify (AlmostFull _ _ handicap) = if handicap == 4 then Just "No Handicap Parking" else Nothing
handicapStringify (Full _ handicap) = if handicap == 4 then Just "No Handicap Parking" else Nothing

stringifyState :: CarParkState -> String
stringifyState state = (singleStringify state) ++ (foldMap (\x -> "/" ++ x) $ handicapStringify state)

main =
    do putStrLn "Enter instructions: "
       instructions <- getLine
       putStrLn (stringify $ entry instructions)

