import Control.Monad (guard, replicateM)
import Data.Bits ((.|.), setBit, clearBit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

data BitMask = BitMask { ones :: Integer, floating :: [Int] }

startBitMask :: BitMask
startBitMask = BitMask 0 []

readBitMask :: String -> BitMask
readBitMask string = BitMask nones nfloating where
	nones = sum $ do
		(bitval,char) <- zip ((2^) <$> [35,34..0]) string
		guard $ char == '1'
		return bitval
	nfloating = do
		(bitpos,char) <- zip [35,34..0] string
		guard $ char == 'X'
		return bitpos

applyMask :: BitMask -> Integer -> [Integer]
applyMask mask val = do
	rawfunctions <- replicateM (length $ floating mask)
		[flip setBit, flip clearBit]
	let functions = zipWith ($) rawfunctions $ floating mask
	return $ foldl (flip ($)) (val .|. ones mask) functions

data ProgramState = ProgramState
	{ values :: Map Integer Integer, mask :: BitMask }

startProgramState :: ProgramState
startProgramState = ProgramState M.empty startBitMask

updateMask :: ProgramState -> String -> ProgramState
updateMask state string = state { mask = readBitMask string }

setValue :: ProgramState -> Integer -> Integer -> ProgramState
setValue state address value = state {
	values =
		M.fromList ((,) <$> applyMask (mask state) address <*> [value]) <>
		values state
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
