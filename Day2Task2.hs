import Data.List ((!!))

validate :: Eq a => (Int, Int, a, [a]) -> Bool
validate (firstOcc, secondOcc, letter, string) =
	is firstOcc /= is secondOcc where
		is occ = string !! (occ - 1) == letter

-- Changing what the variables mean ruins my variable names >:(
parseQuery :: String -> (Int, Int, Char, String)
parseQuery input = (read minOcc, read maxOcc, letter, string) where
	(occ, _:letter:_:_:string) = span (/= ' ') input
	(minOcc, _:maxOcc) = span (/='-') occ

main :: IO ()
main = interact $ show . length . filter validate . fmap parseQuery . lines
