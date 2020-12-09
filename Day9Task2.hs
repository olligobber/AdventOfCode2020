import Data.Set (Set)
import qualified Data.Set as S

isSumOfTwo :: [Integer] -> Integer -> Bool
isSumOfTwo list total = case foldl processOne (Just S.empty) list of
	Nothing -> True
	Just _ -> False
	where
		processOne :: Maybe (Set Integer) -> Integer -> Maybe (Set Integer)
		processOne Nothing _ 	= Nothing
		processOne (Just s) i
			| i `S.member` s	= Nothing
			| i+i == total 		= Just s
			| otherwise 		= Just $ S.insert (total-i) s

whichNotSumOfPrev :: [Integer] -> Integer
whichNotSumOfPrev fullList =
	case foldl processOne (Right $ take 25 fullList) (drop 25 fullList) of
		Left i -> i
		Right _ -> error "None found"
	where
		processOne ::
			Either Integer [Integer] -> Integer -> Either Integer [Integer]
		processOne (Left i) _ 		= Left i
		processOne (Right prevs) cur
			| isSumOfTwo prevs cur 	= Right $ tail prevs ++ [cur]
			| otherwise				= Left cur

findRangeSumTo :: Integer -> [Integer] -> [Integer]
findRangeSumTo target list = find 0 [] list where
	find :: Integer -> [Integer] -> [Integer] -> [Integer]
	find cursum curlist unadded
		| cursum == target 	= curlist
		| cursum < target 	= find
			(cursum + head unadded) (curlist ++ [head unadded]) (tail unadded)
		| otherwise 	= find (cursum - head curlist) (tail curlist) unadded

main :: IO ()
main = interact $ show . ((+) <$> maximum <*> minimum) .
	(whichNotSumOfPrev >>= findRangeSumTo) . fmap read . lines
