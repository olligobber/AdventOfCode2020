import Data.List (sort)

main :: IO ()
main = interact $
	show .
	((*) <$> length . filter (==1) <*> length . filter (==3)) .
	(\x -> zipWith (-) (x++[last x + 3]) (0:x)) .
	sort .
	fmap read .
	lines
