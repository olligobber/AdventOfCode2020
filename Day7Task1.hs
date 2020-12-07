import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)

-- adjective bag -> adjective
readBag :: String -> String
readBag = unwords . init . words

-- number adjective bag -> adjective
readQuantBag :: String -> String
readQuantBag = unwords . tail . init . words

-- given a line of input, make a map from outer to inner
readContainment :: String -> (String, [String])
readContainment line = (outer, inner) where
	outerBag:innerBags:_ = splitOn "contain" line
	outer = readBag outerBag
	inner
		| innerBags == " no other bags." = []
		| otherwise = readQuantBag <$> splitOn "," innerBags

-- read all containments of bags into a map from outer to inner
readContainments :: String -> Map String [String]
readContainments = M.fromListWith undefined . fmap readContainment . lines

{-	Using a map from one element to its contents, check if the second
	contains the first -}
contains :: (Eq k, Foldable f) => (k -> f k) -> k -> k -> Bool
contains iter goal current =
	any (== goal) (iter current) || any (contains iter goal) (iter current)

-- Get all things in the map that contain that element eventually
getContains :: Ord k => k -> Map k [k] -> [k]
getContains goal m = filter (contains (m M.!) goal) $ M.keys m

main :: IO ()
main = interact $ show . length . getContains "shiny gold" . readContainments
