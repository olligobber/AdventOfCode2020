import Data.List ((!!))

-- Check if row number n contains a tree
checkRow :: Int -> Int -> Int -> String -> Bool
checkRow dx dy rowNum rowString = validRow && validCol where
	validRow = rowNum `mod` dy == 0
	jumpNum = rowNum `div` dy
	col = jumpNum * dx
	width = length rowString
	modcol = col `mod` width
	contents = rowString !! modcol
	validCol = contents == '#'

countRows :: Int -> Int -> [String] -> Int
countRows dx dy = length . filter id . zipWith (checkRow dx dy) [0..]

doSlopes :: [String] -> Int
doSlopes = do
	first <- countRows 1 1
	second <- countRows 3 1
	third <- countRows 5 1
	fourth <- countRows 7 1
	fifth <- countRows 1 2
	return $ first * second * third * fourth * fifth

main :: IO ()
main = interact $ show . doSlopes . lines
