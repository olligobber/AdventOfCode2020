import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)
import Control.Applicative (liftA2)

-- adjective bag -> adjective
readBag :: String -> String
readBag = unwords . init . words

-- number adjective bag -> (number, adjective)
readQuantBag :: String -> (Int, String)
readQuantBag string = (read quant, unwords $ init rest) where
	quant:rest = words string

-- given a line of input, make a map from outer bags to inner bags
readContainment :: String -> (String, [(Int,String)])
readContainment line = (outer, inner) where
	outerBag:innerBags:_ = splitOn "contain" line
	outer = readBag outerBag
	inner
		| innerBags == " no other bags." = []
		| otherwise = readQuantBag <$> splitOn "," innerBags

-- read all containments of bags into a map from outer to inner
readContainments :: String -> Map String [(Int,String)]
readContainments = M.fromListWith undefined . fmap readContainment . lines

bagsContained :: Ord k => Map k [(Int,k)] -> Map k Int
bagsContained = bagsContained' M.empty where
	bagsContained' :: Ord k => Map k Int -> Map k [(Int,k)] -> Map k Int
	bagsContained' known unknown
		| M.null newUnknown = newKnown <> known
		| otherwise = bagsContained' (newKnown <> known) newUnknown
		where
			newKnown = M.mapMaybe
				(foldl (liftA2 (+)) (Just 0) . fmap unpackbag) unknown
			unpackbag (a, bag) = (+a) . (a*) <$> M.lookup bag known
			newUnknown = M.withoutKeys unknown $ M.keysSet newKnown

main :: IO ()
main =
	interact $ show . M.lookup "shiny gold" . bagsContained . readContainments
