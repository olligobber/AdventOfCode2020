validate :: Eq a => (Int, Int, a, [a]) -> Bool
validate (minOcc, maxOcc, letter, string) =
	minOcc <= occ && occ <= maxOcc where
		occ = length $ filter (== letter) string

parseQuery :: String -> (Int, Int, Char, String)
parseQuery input = (read minOcc, read maxOcc, letter, string) where
	(occ, _:letter:_:_:string) = span (/= ' ') input
	(minOcc, _:maxOcc) = span (/='-') occ

main :: IO ()
main = interact $ show . length . filter validate . fmap parseQuery . lines
