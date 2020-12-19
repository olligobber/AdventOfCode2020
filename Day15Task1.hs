import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

data GameState = GameState {
	turnSaid :: Map Integer Integer,
	lastSaid :: Integer,
	turn :: Integer
} deriving Show

startGame :: [Integer] -> GameState
startGame startNumbers = GameState
	(M.fromList $ zip (init startNumbers) [1..])
	(last startNumbers)
	(toInteger $ length startNumbers)

doTurn :: GameState -> GameState
doTurn state = GameState
	(M.insert (lastSaid state) (turn state) (turnSaid state))
	(turn state -
		M.findWithDefault (turn state) (lastSaid state) (turnSaid state))
	(turn state + 1)

doUntil2020 :: GameState -> Integer
doUntil2020 state
	| turn state == 2020 = lastSaid state
	| turn state > 2020 = undefined
	| otherwise = doUntil2020 $ doTurn state

main :: IO ()
main = interact $ show . doUntil2020 . startGame . fmap read . splitOn ","
