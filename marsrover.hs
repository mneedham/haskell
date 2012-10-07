data Position = Position Int Int Direction
	deriving (Show, Eq, Read)
data Direction = N | E | W | S
	deriving (Show, Eq, Enum, Bounded, Read)
data Command = L | R | M
	deriving (Show, Eq, Enum, Bounded, Read)

data DeltaPosition = DeltaPosition Int Int deriving (Show)

data State = State {
	leftDirection :: Direction,
	rightDirection :: Direction,
	delta :: DeltaPosition
} deriving (Show)

toPosition input = read ("Position " ++ input) :: Position

toList x = [x]

toCommands input = map (read . toList) input

state N = State W E (DeltaPosition 0 (-1))
state W = State S N (DeltaPosition (-1) 0)
state S = State E W (DeltaPosition 0 1)
state E = State N S (DeltaPosition 1 0)

add (Position x y d) (DeltaPosition x' y') = Position (x + x') (y + y') d

execute (Position x y d) L = Position x y (leftDirection $ state d)
execute (Position x y d) R = Position x y (rightDirection $ state d)
execute p@(Position x y d) M = add p (delta $ state d)

route position commands = foldl execute initialPosition (toCommands commands)
	where initialPosition = toPosition position