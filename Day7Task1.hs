import Data.Set (Set)
import Data.Set as S
import Data.Map.Lazy (Map)
import Data.Map.Lazy as M
import Data.List.Split (splitOn)

-- adjective bag -> adjective
readBag :: String -> String
readBag = unwords . init . words

-- number adjective bag -> adjective
readQuantBag :: String -> String
readQuantBag = unwords . tail . init . words

-- given a line of input, make a map from inner bags to outer bags
readContainment :: String -> Map String (Set String)
readContainment line = M.fromSet (const $ S.singleton outer) inner where
	outerBag:innerBags:_ = splitOn "contain" line
	outer = readBag outerBag
	inner
		| innerBags == " no other bags." = S.empty
		| otherwise = S.fromList $ readQuantBag <$> splitOn "," innerBags

-- read all containments of bags into a map from inner to outer
readContainments :: String -> Map String (Set String)
readContainments = M.unionsWith (<>) . fmap readContainment . lines

closure :: Ord k => Set k -> Map k (Set k) -> Set k
closure start transition
	| next `S.isSubsetOf` start = start
	| otherwise = closure (next <> start) transition
	where
		next = foldMap (\k -> M.findWithDefault S.empty k transition) start

main :: IO ()
main = interact $ show .
	(+(-1)) . length . closure (S.singleton "shiny gold") . readContainments
