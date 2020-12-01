import Control.Monad (guard)

findSum :: [Int] -> (Int, Int)
findSum list = head $ do
	x <- list
	y <- list
	guard $ x + y == 2020
	return (x,y)

main :: IO ()
main = interact $ show . uncurry (*) . findSum . fmap read . lines
