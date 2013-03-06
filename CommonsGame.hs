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
currentPlay = (M.fromList [(Red,3),(Green,3),(Black,2)]) :: Map GameCard Int
currentState = (0, 100) :: GameState
numPlayers = 8

_rewardFromNumPlayed played table = head $ drop (played ! Red) table


_scoreRed :: GamePlay -> GameTable -> Double
_scoreRed played table
    | (M.member Red played) = fromIntegral $ fst $ _rewardFromNumPlayed played table
    | otherwise = 0.0

main :: IO ()
main = putStrLn "Hello world!"
