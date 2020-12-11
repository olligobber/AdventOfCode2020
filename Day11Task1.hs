import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (guard)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = do
	dx <- [-1..1]
	dy <- [-1..1]
	guard $ (dx,dy) /= (0,0)
	return (x+dx, y+dy)

-- Determine if a seat is occupied on the next round
isOccupied :: Map (Int, Int) Bool -> (Int, Int) -> Bool
isOccupied m p = case M.lookup p m of
	Nothing -> undefined
	Just True -> neighbourSeats < 4
	Just False -> neighbourSeats == 0
	where
		neighbourSeats =
			length $ filter (flip (M.findWithDefault False) m) $ neighbours p

-- Iterate seat occupation
iter :: Map (Int, Int) Bool -> Map (Int, Int) Bool
iter = M.fromSet <$> isOccupied <*> M.keysSet

fixed :: Eq x => (x -> x) -> x -> x
fixed f x
	| f x == x = x
	| otherwise = fixed f $ f x

readSeats :: String -> Map (Int, Int) Bool
readSeats s = M.fromList $ do
	(y, line) <- zip [0..] $ lines s
	(x, char) <- zip [0..] line
	case char of
		'#' -> pure ((x,y),True)
		'L' -> pure ((x,y),False)
		'.' -> []
		_ -> undefined

main :: IO ()
main = interact $ show . length . M.filter id . fixed iter . readSeats
