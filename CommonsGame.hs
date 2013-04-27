import Data.Map as M
import Data.List as L

-- denotes the current state of the game
type GameState = (Iteration, Marker)

-- denotes the current iteration of the game. the number of times
-- cards have been played in this instance of the game.
type Iteration = Int

-- denotes the position of marker in the game.
type Marker = Int

-- denotes the last play made by all the players
type GamePlay = M.Map GameCard Int

-- denotes the rewards for red and green players
type GameTable = [(RedReward, GreenReward)]

type GreenReward = Int
type RedReward = Int

-- denotes the cards available for play
data GameCard = Red
              | Green
              | Yellow
              | Orange
              | Black deriving (Show, Eq, Ord)

-- some test values here
currentTable = [(0, 100), (40, 102), (40, 102), (40,102), (44, 106), (46, 108), (48, 110), (50, 0)] :: GameTable
currentPlay = M.fromList [(Red,3),(Green,3),(Black,2)] :: Map GameCard Int
currentState = (0, 100) :: GameState
numPlayers = 8 :: Int

_rewardFromNumPlayed played table = head $ drop (played ! Red) table

score :: GameCard -> GamePlay -> GameTable -> Double
score Black played table
    | M.member Black played = -8.0 / (fromIntegral $ played ! Black)
    | otherwise = 0.0

score Green played table
    | M.member Green played && M.member Black played = -20.0
    | M.member Green played = fromIntegral $ snd $ _rewardFromNumPlayed played table
    | otherwise = 0.0

score Red played table
    | M.member Red played && M.member Orange played = _orangeBenefit + _partialRedScore
    | M.member Red played = _partialRedScore
    | otherwise = 0.0
    where
        _partialRedScore = fromIntegral $ fst $ _rewardFromNumPlayed played table
        _orangeBenefit = 10.0 * (fromIntegral $ played ! Orange)

score Yellow played table
    | M.member Yellow played = 6.0
    | otherwise = 0.0

score Orange played table
    | M.member Orange played = -8.0 / (fromIntegral $ played ! Orange)
    | otherwise = 0.0

_updateMarker :: GamePlay -> Int -> Int -> Int
_updateMarker played turn m
    | mod turn 6 == 0 = m - gc + 0 {-- the zero should be a random value --}
    | otherwise = m - gc
    where
        gc
            | M.member Green played  = played ! Green
            | otherwise = 0

main :: IO ()
main = putStrLn "Hello world!"
