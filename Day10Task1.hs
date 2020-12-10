import Data.List (sort)

main :: IO ()
main = interact $
	show .
	((*) <$> length . filter (==1) <*> length . filter (==3)) .
	(3:) .
	(zipWith (-) <$> id <*> (0:)) .
	sort .
	fmap read .
	lines
