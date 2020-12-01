import Control.Monad (guard)

findSum :: [Int] -> [Int]
findSum list = head $ do
	x <- list
	y <- list
	z <- list
	guard $ x + y + z == 2020
	return [x,y,z]

main :: IO ()
main = interact $ show . product . findSum . fmap read . lines
