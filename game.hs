module Game where

data Color = Red | Black | Blue | White | Green
    deriving (Show, Eq, Ord)

data Card = Card {
    point :: Int
    ,color :: Color
    ,cost :: [Int]
} deriving (Show)

data Player = Player {
    tokens :: [Int]
    ,cards :: [Int]
    ,points :: Int
} deriving (Show)

toColorList :: Color -> [Int]
toColorList color = case color of
    Red -> [1,0,0,0,0]
    Black -> [0,1,0,0,0]
    Blue -> [0,0,1,0,0]
    White -> [0,0,0,1,0]
    Green -> [0,0,0,0,1]

canPurchase :: Player -> Card -> Bool
canPurchase player card =
    let temp = zipWith (-) (tokens player) (cost card)
        result = [a | a <- temp, a < 0]
    in null result

purchase :: Player -> Card -> Player
purchase player card =
    if canPurchase player card
    then Player {
        tokens = zipWith (-) (tokens player) (cost card)
        ,cards = zipWith (+) (cards player) (toColorList (color card))
        ,points = points player + point card
    }
    else player

run =
    let card = Card {point = 1, color = Red, cost = [2,1,0,0,0]}
        player = Player {tokens = [3,2,0,0,1], cards = [0,0,0,0,0], points = 0}
    in print (purchase player card)