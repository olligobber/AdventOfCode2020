import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (guard)
import Data.Foldable (fold, toList)
import Data.Maybe (isJust)

data Seat = None | Empty | Filled deriving Eq

instance Semigroup Seat where
	None <> x = x
	x <> _ = x

instance Monoid Seat where
	mempty = None

scale :: (Int,Int) -> Int -> (Int,Int)
scale (x,y) a = (a*x,a*y)

add :: (Int, Int) -> (Int, Int) -> (Int,Int)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

directions :: [(Int, Int)]
directions = do
	dx <- [-1..1]
	dy <- [-1..1]
	guard $ (dx,dy) /= (0,0)
	return (dx,dy)

occupiedSight :: Map (Int,Int) Seat -> (Int,Int) -> (Int,Int) -> Bool
occupiedSight m start go = (==Filled) $ fold $ foldMap toList $
	takeWhile isJust $ flip M.lookup m . add start . scale go <$> [1..]

-- Determine if a seat is occupied on the next round
isOccupied :: Map (Int, Int) Seat -> (Int, Int) -> Seat
isOccupied m p = case m M.! p of
	Empty | visibleSeats == 0 -> Filled
	Filled | visibleSeats >= 5 -> Empty
	x -> x
	where
		visibleSeats = length $ filter (occupiedSight m p) directions

-- Iterate seat occupation
iter :: Map (Int, Int) Seat -> Map (Int, Int) Seat
iter = M.fromSet <$> isOccupied <*> M.keysSet

fixed :: Eq x => (x -> x) -> x -> x
fixed f x
	| f x == x = x
	| otherwise = fixed f $ f x

readSeats :: String -> Map (Int, Int) Seat
readSeats s = M.fromList $ do
	(y, line) <- zip [0..] $ lines s
	(x, char) <- zip [0..] line
	case char of
		'#' -> pure ((x,y),Filled)
		'L' -> pure ((x,y),Empty)
		'.' -> pure ((x,y),None)
		_ -> undefined

main :: IO ()
main = interact $ show . length . M.filter (==Filled) . fixed iter . readSeats
