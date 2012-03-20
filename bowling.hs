data Throw = Normal Int | Strike deriving (Show) 

data Hangover = None | Throw deriving (Show)

scoreMe bowls hangover

scoreme :: [Int] -> Hangover -> [Throw]
scoreme (x:xs) (Normal score) = if score + x == 10 then Spare


isStrike throw = throw == 10
isSpare throw1 throw2 = throw1 + throw2 == 10

tallyStrike :: Throw -> Throw -> Throw -> Int
tallyStrike previous (Normal t1) (Normal t2) = 10 + t1 + t2
tallyStrike previous Strike (Normal t2) = 10 + 10 + t2
tallyStrike previous (Normal t1) Strike = 10 + t1 + 10

score :: [Int] -> Int
score bowls = sum bowls