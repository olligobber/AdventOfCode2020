import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)

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

-- find out how many bags one bag contains
bagsContained :: Ord k => k -> Map k [(Int,k)] -> Int
bagsContained bag m = sum $ inBag <$> m M.! bag where
	inBag (amount,inner) = amount * (bagsContained inner m + 1)

main :: IO ()
main = interact $ show . bagsContained "shiny gold" . readContainments
