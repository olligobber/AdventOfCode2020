import Data.List ((!!))

-- Check if row number n contains a tree
checkRow :: Int -> String -> Bool
checkRow num row = tree where
	col = num * 3 -- col 0 = row 0, then add 3 for each row after that
	width = length row
	modcol = col `mod` width
	contents = row !! modcol
	tree = contents == '#'

countRows :: [String] -> Int
countRows = length . filter id . zipWith checkRow [0..]

main :: IO ()
main = interact $ show . countRows . lines
