import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set as S

processGroup :: String -> Int
processGroup = length . foldMap id . fmap S.fromList . lines

main :: IO ()
main = interact $
	show . sum . fmap processGroup . splitOn "\n\n"
