import System.Random

data PointWinner = Player1 | Player2 deriving (Show)

data PointScore = Love | Fifteen | Thirty | Forty deriving (Show)

data GameScore = Normal PointScore PointScore | Deuce | Advantage PointWinner | Game PointWinner deriving (Show)

updateScore :: GameScore -> PointWinner -> GameScore

updateScore (Advantage Player1) Player1 = Game Player1
updateScore (Advantage Player2) Player2 = Game Player2
updateScore (Advantage Player2) Player1 = Deuce
updateScore (Advantage Player1) Player2 = Deuce

updateScore (Deuce) player = Advantage player

updateScore (Normal Thirty Forty) Player1 = Deuce
updateScore (Normal Forty Thirty) Player2 = Deuce

updateScore (Normal Love player2) Player1 = Normal Fifteen player2
updateScore (Normal Fifteen player2) Player1 = Normal Thirty player2
updateScore (Normal Thirty player2) Player1 = Normal Forty player2
updateScore (Normal Forty _) Player1 = Game Player1

updateScore (Normal player1 Love) Player2 = Normal player1 Fifteen
updateScore (Normal player1 Fifteen) Player2 = Normal player1 Thirty
updateScore (Normal player1 Thirty) Player2 = Normal player1 Forty
updateScore (Normal player1 Forty) Player2 = Game Player2

updateScore (Game _) _ = error "Game is finished!"

rand :: IO Int
rand = getStdRandom (randomR (1, 10))