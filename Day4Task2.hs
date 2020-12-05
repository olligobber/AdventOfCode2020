import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as M
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Control.Monad (guard)

splitOnce :: Eq a => a -> [a] -> ([a],[a])
splitOnce splitter string = (first, second) where
	(first, _:second) = span (/= splitter) string

toPassport :: String -> Map String String
toPassport = M.fromList . fmap (splitOnce ':') . words

validatePassport :: Map String String -> Bool
validatePassport passport = isJust $ do
	byr <- passport !? "byr" >>= readMaybe
	guard $ byr >= 1920
	guard $ byr <= 2002
	iyr <- passport !? "iyr" >>= readMaybe
	guard $ iyr >= 2010
	guard $ iyr <= 2020
	eyr <- passport !? "eyr" >>= readMaybe
	guard $ eyr >= 2020
	guard $ eyr <= 2030
	hgt <- passport !? "hgt"
	let hgtlen = length hgt
	hgtnum <- readMaybe $ take (hgtlen-2) hgt
	let hgtunit = drop (hgtlen-2) hgt
	case hgtunit of
		"cm" -> do
			guard $ hgtnum >= 150
			guard $ hgtnum <= 193
		"in" -> do
			guard $ hgtnum >= 59
			guard $ hgtnum <= 76
		_ -> Nothing
	'#':hcl <- passport !? "hcl"
	guard $ length hcl == 6
	guard $ all (`elem` "0123456789abcdef") hcl
	ecl <- passport !? "ecl"
	guard $ ecl `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
	pid <- passport !? "pid"
	guard $ length pid == 9
	guard $ all (`elem` "0123456789") pid

main :: IO ()
main = interact $
	show . length . filter (validatePassport . toPassport) . splitOn "\n\n"
