import Data.VectorSpace ((*^))
import Data.AdditiveGroup ((^+^))
import Data.Function (on)
import Data.List (uncons)
import Data.Maybe (fromMaybe)

data BoatState = BoatState {
	waypoint :: (Int,Int),
	position :: (Int,Int)
} deriving Show

toVector :: Char -> (Int,Int)
toVector 'N' = (0,1)
toVector 'E' = (1,0)
toVector 'S' = (0,-1)
toVector 'W' = (-1,0)

left :: (Int,Int) -> (Int,Int)
left (x,y) = (-y,x)

right :: (Int,Int) -> (Int,Int)
right (x,y) = (y,-x)

times :: Int -> (a -> a) -> a -> a
times 0 _ x = x
times n f x = times (n-1) f $ f x

move :: BoatState -> (Char,Int) -> BoatState
move b ('L',r) = BoatState (times (r`div`90) left $ waypoint b) (position b)
move b ('R',r) = BoatState (times (r`div`90) right $ waypoint b) (position b)
move b ('F',d) = BoatState (waypoint b) (d *^ waypoint b ^+^ position b)
move b (c,d) = BoatState (d *^ toVector c ^+^ waypoint b) (position b)

startBoat :: BoatState
startBoat = BoatState (10,1) (0,0)

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
