import Control.Monad (guard)
import Data.Bits ((.&.),(.|.))
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

data BitMask = BitMask { zeros :: Integer, ones :: Integer }

startBitMask :: BitMask
startBitMask = BitMask (2^36 - 1) 0

readBitMask :: String -> BitMask
readBitMask string = BitMask nzeros nones where
	bitvals = (2^) <$> [35,34..0]
	nzeros = sum $ do
		(bitval,char) <- zip bitvals string
		guard $ char /= '0'
		return bitval
	nones = sum $ do
		(bitval,char) <- zip bitvals string
		guard $ char == '1'
		return bitval

applyMask :: BitMask -> Integer -> Integer
applyMask mask = (.|. ones mask) . (.&. zeros mask)

data ProgramState = ProgramState
	{ values :: Map Integer Integer, mask :: BitMask }

startProgramState :: ProgramState
startProgramState = ProgramState M.empty startBitMask

updateMask :: ProgramState -> String -> ProgramState
updateMask state string = state { mask = readBitMask string }

setValue :: ProgramState -> Integer -> Integer -> ProgramState
setValue state address value = state {
	values = M.insert address (applyMask (mask state) value) (values state)
	}

updateState :: ProgramState -> String -> ProgramState
updateState state ('m':'a':_:_:_:_:_:mask) = updateMask state mask
updateState state ('m':'e':_:_:rest) = setValue state address value where
	address:value:_ = read <$> splitOn "] = " rest

main :: IO ()
main = interact $
	show .
	sum .
	values .
	foldl updateState startProgramState .
	lines
