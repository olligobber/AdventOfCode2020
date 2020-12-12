import Data.VectorSpace ((*^))
import Data.AdditiveGroup ((^+^))
import Data.Function (on)
import Data.List (uncons)
import Data.Maybe (fromMaybe)

data Direction = North | East | South | West deriving Enum

data BoatState = BoatState {
	rotation :: Direction,
	position :: (Int,Int)
}

toVector :: Direction -> (Int,Int)
toVector North = (0,1)
toVector East = (1,0)
toVector South = (0,-1)
toVector West = (-1,0)

left :: Direction -> Direction
left North = West
left x = pred x

right :: Direction -> Direction
right West = North
right x = succ x

times :: Int -> (a -> a) -> a -> a
times 0 _ x = x
times n f x = times (n-1) f $ f x

readDirection :: Char -> Direction
readDirection 'N' = North
readDirection 'E' = East
readDirection 'S' = South
readDirection 'W' = West

move :: BoatState -> (Char,Int) -> BoatState
move b ('L',r) = BoatState (times (r`div`90) left $ rotation b) (position b)
move b ('R',r) = BoatState (times (r`div`90) right $ rotation b) (position b)
move b ('F',d) =
	BoatState (rotation b) (d *^ toVector (rotation b) ^+^ position b)
move b (c,d) =
	BoatState (rotation b) (d *^ toVector (readDirection c) ^+^ position b)

startBoat :: BoatState
startBoat = BoatState East (0,0)

main :: IO ()
main = interact $
	show .
	uncurry ((+) `on` abs) .
	position .
	foldl move startBoat .
	fmap (
		fmap read .
		fromMaybe ('F',"0") .
		uncons
	) .
	lines
