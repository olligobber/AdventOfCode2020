import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Array.IO (IOArray)
import qualified Data.Array.MArray as A
import Data.IORef (IORef)
import qualified Data.IORef as R

turns :: Integer
turns = 3*10^7

data GameState = GameState {
	turnSaid :: IOArray Integer (Maybe Integer),
	lastSaid :: IORef Integer,
	turn :: IORef Integer
}

startGame :: [Integer] -> IO GameState
startGame startNumbers = do
	array <- A.newArray (0,turns) Nothing
	mapM_ (uncurry $ A.writeArray array) $
		zip (init startNumbers) (Just <$> [1..])
	GameState array
		<$> R.newIORef (last startNumbers)
		<*> R.newIORef (toInteger $ length startNumbers)

doTurn :: GameState -> IO ()
doTurn state = do
	lastSpoken <- R.readIORef $ lastSaid state
	currentTurn <- R.readIORef $ turn state
	turnSpoken <- A.readArray (turnSaid state) lastSpoken
	let nextSpoken = case turnSpoken of
			Nothing -> 0
			Just n -> currentTurn - n
	A.writeArray (turnSaid state) lastSpoken (Just currentTurn)
	R.writeIORef (lastSaid state) nextSpoken
	R.modifyIORef (turn state) succ

doUntil :: GameState -> IO Integer
doUntil state = do
	currentTurn <- R.readIORef $ turn state
	if currentTurn == turns then
		R.readIORef $ lastSaid state
	else
		doTurn state >> doUntil state

main :: IO ()
main = (fmap read . splitOn "," <$> getLine) >>= startGame >>= doUntil >>= print
