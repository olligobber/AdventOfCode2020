import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)

splitOnce :: Eq a => a -> [a] -> ([a],[a])
splitOnce splitter string = (first, second) where
	(first, _:second) = span (/= splitter) string

toPassport :: String -> Map String String
toPassport = M.fromList . fmap (splitOnce ':') . words

validatePassport :: Map String String -> Bool
validatePassport passport =
	all (`M.member` passport) ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

main :: IO ()
main = interact $
	show . length . filter (validatePassport . toPassport) . splitOn "\n\n"
