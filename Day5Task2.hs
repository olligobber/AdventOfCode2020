import Data.List ((\\))

startRowRange :: (Int, Int)
startRowRange = (0,127)

reduceRowRange :: (Int, Int) -> Char -> (Int, Int)
reduceRowRange (a,b) 'F' = (a, (a+b) `div` 2)
reduceRowRange (a,b) 'B' = (((a+b) `div` 2) + 1, b)
reduceRowRange _ _ = error "Not F/B"

startColRange :: (Int, Int)
startColRange = (0,7)

reduceColRange :: (Int, Int) -> Char -> (Int, Int)
reduceColRange (a,b) 'L' = (a, (a+b) `div` 2)
reduceColRange (a,b) 'R' = (((a+b) `div` 2) + 1, b)
reduceColRange _ _ = error "Not R/L"

getRow :: String -> Int
getRow = fst . foldl reduceRowRange startRowRange

getCol :: String -> Int
getCol = fst . foldl reduceColRange startColRange

getSeatId :: String -> Int
getSeatId s = row * 8 + col where
	row = getRow $ take 7 s
	col = getCol $ drop 7 s

findEmptySeat :: [Int] -> Int
findEmptySeat taken = head $ [minL .. maxL] \\ taken where
	minL = minimum taken
	maxL = maximum taken

main :: IO ()
main = interact $ show . findEmptySeat . fmap getSeatId . lines
