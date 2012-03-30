import System.Random
import Data.List

data Player = Player1 | Player2 deriving (Show)
data PointScore = Love | Fifteen | Thirty | Forty deriving (Show)
data GameScore = Normal PointScore PointScore | Deuce | Advantage Player | Game Player deriving (Show)
data SetScore = NormalSetScore Int Int | Set Player deriving (Show)
data MatchScore = NormalMatchScore Int Int | Match Player deriving (Show)

data Overall = OverallScore MatchScore SetScore GameScore deriving (Show)

updateMatchScore :: MatchScore -> Player -> MatchScore
updateMatchScore (NormalMatchScore player1 player2) Player1  = NormalMatchScore (player1 + 1) player2
updateMatchScore (NormalMatchScore player1 player2) Player2  = NormalMatchScore player1 (player2 + 1)

updateSetScore :: SetScore -> Player -> SetScore
updateSetScore (NormalSetScore player1 player2) Player1  | player1 >=4 && player2 >=4 && player1 > player2 = Set Player1 
updateSetScore (NormalSetScore player1 player2) Player2  | player1 >=4 && player2 >=4 && player2 > player1 = Set Player2 
updateSetScore (NormalSetScore player1 5) Player1  = Set Player1
updateSetScore (NormalSetScore player1 5) Player2  = Set Player2

updateSetScore (NormalSetScore player1 player2) Player1  = NormalSetScore (player1 + 1) player2
updateSetScore (NormalSetScore player1 player2) Player2  = NormalSetScore player1 (player2 + 1)

newGame = (Normal Love Love)
newSet = (NormalSetScore 0 0)

updatePointScore :: GameScore -> Player -> GameScore
updatePointScore (Advantage Player2) Player1 = Deuce
updatePointScore (Advantage Player1) Player2 = Deuce

updatePointScore (Deuce) player = (Advantage player)

updatePointScore (Normal Thirty Forty) Player1 = Deuce
updatePointScore (Normal Forty Thirty) Player2 = Deuce

updatePointScore (Normal Love player2) Player1 =  (Normal Fifteen player2)
updatePointScore (Normal Fifteen player2) Player1 = (Normal Thirty player2)
updatePointScore (Normal Thirty player2) Player1 = (Normal Forty player2)

updatePointScore (Normal player1 Love) Player2 = (Normal player1 Fifteen)
updatePointScore (Normal player1 Fifteen) Player2 = (Normal player1 Thirty)
updatePointScore (Normal player1 Thirty) Player2 = (Normal player1 Forty)

update :: Overall -> Player -> Overall
update (OverallScore matchScore (NormalSetScore player1 player2) _) Player1
	| player1 >=4 && player2 >=4 && player1 > player2 = OverallScore (updateMatchScore matchScore Player1) newSet newGame
	| player1 >=4 && player2 >=4 && player2 > player1 = OverallScore (updateMatchScore matchScore Player2) newSet newGame

update (OverallScore matchScore setScore (Advantage Player1)) Player1 = OverallScore matchScore (updateSetScore setScore Player1) newGame
update (OverallScore matchScore setScore (Advantage Player2)) Player2 = OverallScore matchScore (updateSetScore setScore Player2) newGame
update (OverallScore matchScore setScore (Normal player1 Forty)) Player2 = OverallScore matchScore (updateSetScore setScore Player2) newGame
update (OverallScore matchScore setScore (Normal Forty _)) Player1 = OverallScore matchScore (updateSetScore setScore Player1) newGame

update (OverallScore matchScore setScore gameScore) player = OverallScore matchScore setScore (updatePointScore gameScore player)

noScore = OverallScore (NormalMatchScore 0 0) (NormalSetScore 0 0) (Normal Love Love)
go = Prelude.scanl (\score  point -> update score point) noScore player1MostlyWins

player1MostlyWins = Data.List.foldl (++) []  [[Player1, Player2, Player1] | x <- [1..100]]

--how do I generate a sequence of Player1's?
--how do I alternate between Player1 winning and then Player2?