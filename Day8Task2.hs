import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid (First(..))

data Instruction =
	Acc Integer |
	Jmp Integer |
	Nop Integer |
	Terminate

data HandheldState = HandheldState {
	code :: Map Integer Instruction,
	pointer :: Integer,
	accumulator :: Integer,
	hasRun :: Set Integer
	}

makeState :: [Instruction] -> HandheldState
makeState instructions = HandheldState
	(M.fromList $ zip [0..] $ instructions ++ [Terminate])
	0
	0
	S.empty

advanceState :: HandheldState -> Either (Maybe Integer) HandheldState
advanceState state
	| pointer state `S.member` hasRun state = Left Nothing
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
		Just (Nop _) -> Right $ HandheldState
			(code state)
			(pointer state + 1)
			(accumulator state)
			(S.insert (pointer state) (hasRun state))
		Just Terminate -> Left $ Just $ accumulator state

run :: [Instruction] -> Maybe Integer
run instructions = runForever $ makeState instructions where
	runForever :: HandheldState -> Maybe Integer
	runForever state = case advanceState state of
		Left result -> result
		Right newstate -> runForever newstate

readInstruction :: String -> Instruction
readInstruction s = case instruction of
	"acc" -> Acc argument
	"jmp" -> Jmp argument
	"nop" -> Nop argument
	_ -> error "Unknown instruction"
	where
		instruction:(argsign:argmag):_ = words s
		argument = case argsign of
			'+' -> read argmag
			'-' -> - read argmag
			_ -> error "Invalid argument"

flipInstruction :: [Instruction] -> Int -> [Instruction]
flipInstruction list i = take i list ++ [flipOne $ list !! i] ++ drop (i+1) list
	where
		flipOne (Acc x) = Acc x
		flipOne (Jmp x) = Nop x
		flipOne (Nop x) = Jmp x
		flipOne Terminate = Terminate

main :: IO ()
main = do
	originalCode <- fmap readInstruction . lines <$> getContents
	print $ foldMap (First . run . flipInstruction originalCode) [0..]
