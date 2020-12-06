import Data.List.Split (splitOn)
import Data.Set as S
main = interact $ show . sum . fmap (length . foldl1 S.intersection . fmap S.fromList . lines) . splitOn "\n\n"
