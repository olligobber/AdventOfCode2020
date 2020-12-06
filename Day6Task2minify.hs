import Data.List.Split (splitOn)
import Data.List (intersect)
main = interact $ show . sum . fmap (length . foldl1 intersect . lines) . splitOn "\n\n"
