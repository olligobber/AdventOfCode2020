import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S

data Instruction =
	Acc Integer |
	Jmp Integer |
	Nop

data HandheldState = HandheldState {
	code :: Map Integer Instruction,
	pointer :: Integer,
	accumulator :: Integer,
	hasRun :: Set Integer
	}

makeState :: [Instruction] -> HandheldState
makeState instructions = HandheldState
	(M.fromList $ zip [0..] instructions)
	0
	0
	S.empty

advanceState :: HandheldState -> Either Integer HandheldState
advanceState state
	| pointer state `S.member` hasRun state = Left $ accumulator state
	| otherwise = case M.lookup (pointer state) (code state) of
		Nothing -> error "Instruction not found"
		Just (Acc a) -> Right $ HandheldState
			(code state)
			(pointer state + 1)
			(accumulator state + a)
			(S.insert (pointer state) (hasRun state))
		Just (Jmp i) -> Right $ HandheldState
			(code state)
			(pointer state + i)
			(accumulator state)
			(S.insert (pointer state) (hasRun state))
		Just Nop -> Right $ HandheldState
			(code state)
			(pointer state + 1)
			(accumulator state)
			(S.insert (pointer state) (hasRun state))

run :: [Instruction] -> Integer
run instructions = runForever $ makeState instructions where
	runForever :: HandheldState -> Integer
	runForever state = case advanceState state of
		Left result -> result
		Right newstate -> runForever newstate

readInstruction :: String -> Instruction
readInstruction s = case instruction of
	"acc" -> Acc argument
	"jmp" -> Jmp argument
	"nop" -> Nop
	_ -> error "Unknown instruction"
	where
		instruction:(argsign:argmag):_ = words s
		argument = case argsign of
			'+' -> read argmag
			'-' -> - read argmag
			_ -> error "Invalid argument"

main :: IO ()
main = interact $ show . run . fmap readInstruction . lines
