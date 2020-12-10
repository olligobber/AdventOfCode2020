import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (State)
import qualified Control.Monad.State as S

-- given a map of how many routes to smaller numbers, get how many routes to this
routesTo :: Int -> State (Map Int Int) ()
routesTo this = do
	to3before <- S.gets $ M.findWithDefault 0 $ this - 3
	to2before <- S.gets $ M.findWithDefault 0 $ this - 2
	to1before <- S.gets $ M.findWithDefault 0 $ this - 1
	S.modify $ M.insert this $ to3before + to2before + to1before

main :: IO ()
main = interact $
	show .
	snd .
	M.findMax .
	flip S.execState (M.singleton 0 1) .
	mapM routesTo .
	sort .
	fmap read .
	lines
