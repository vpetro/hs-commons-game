import Data.Map as M
import Data.List as L
import System.Random

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
              | Black deriving (Show, Eq, Ord, Enum)

-- some test values here
currentTable = [(0, 100), (40, 102), (40, 102), (40,102), (44, 106), (46, 108), (48, 110), (50, 0)] :: GameTable
currentPlay = (M.fromList [(Red,3),(Green,3),(Black,2)]) :: Map GameCard Int
currentState = (0, 100) :: GameState
numPlayers = 8 :: Int


_rewardFromNumPlayed played table = head $ drop (played ! Red) table


score :: GameCard -> GamePlay -> GameTable -> Double
score Black played table
    | M.member Black played = -8.0 / (fromIntegral $ played ! Black)
    | otherwise = 0.0

score Green played table
    | (M.member Green played) && (M.member Black played) = -20.0
    | (M.member Green played) = fromIntegral $ snd $ _rewardFromNumPlayed played table
    | otherwise = 0.0

score Red played table
    | (M.member Red played) && (M.member Orange played) = _orangeBenefit + _partialRedScore
    | (M.member Red played) = _partialRedScore
    | otherwise = 0.0
    where
        _partialRedScore = fromIntegral $ fst $ _rewardFromNumPlayed played table
        _orangeBenefit = 10.0 * (fromIntegral $ played ! Orange)

score Yellow played table
    | (M.member Yellow played) = 6.0
    | otherwise = 0.0

score Orange played table
    | (M.member Orange played) = -8.0 / (fromIntegral $ played ! Orange)
    | otherwise = 0.0


_updateMarker :: GamePlay -> Int -> Int -> Int -> Int
_updateMarker played turn m i
    | (turn /= 0 && (mod turn 6) == 0) = m - gc + i
    | otherwise = m - gc
    where
        gc
            | M.member Green played  = played ! Green
            | otherwise = 0


randomList low high limit= do
    gen <- newStdGen
    return $ take limit (randomRs (low, high) gen :: [Int])


updateState currentState currentPlay i = (ci + 1, cm') :: GameState
    where
        ci = fst currentState
        cm = snd currentState
        cm' = _updateMarker currentPlay ci cm i


player i = enumFrom Red !! i


groupedCards cls = L.map (\x -> (head x, length x)) cls
cardLists pcs = group $ sort $ L.map (\sp -> head sp) pcs


gameLoop :: GameState -> [[Int]] -> [Int] -> IO ()
gameLoop curState cardIndicies [] = return $ ()
gameLoop curState cardIndicies (i:is) = do
    let playedCards = L.map (\sp -> L.map player sp) cardIndicies
    let cp = M.fromList (groupedCards $ cardLists $ playedCards)
    let cs' = updateState curState cp i
    putStrLn $ show cp ++ ", " ++ show cs' ++ ", " ++ (show i)
    putStrLn $ show $ L.map (\k -> (k, score k cp currentTable)) (M.keys cp)
    gameLoop cs' (L.map (\sp -> tail sp) cardIndicies) is


main :: IO ()
main = do
    improvements <- randomList 1 6 10
    cardIndicies <- sequence $ [randomList 0 4 10 | _ <- [1..8]]
    gameLoop currentState cardIndicies improvements
