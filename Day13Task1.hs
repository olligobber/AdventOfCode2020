import Data.List.Split (splitOn)
import Data.Foldable (toList)
import Text.Read (readMaybe)

getMinSecond :: Ord b => [(a,b)] -> (a,b)
getMinSecond list = head $ filter ((==m) . snd) list where
	m = minimum $ snd <$> list

main :: IO ()
main = do
	curtime <- read <$> getLine
	buses <- foldMap toList . fmap readMaybe . splitOn "," <$> getLine
	print $ uncurry (*) $ getMinSecond $
		((,) <$> id <*> ((-curtime) `mod`)) <$> buses
