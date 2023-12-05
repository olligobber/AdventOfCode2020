import Data.List.Split (splitOn)

data Field =
	Field { amin :: Integer, amax :: Integer, bmin :: Integer, bmax :: Integer }
	deriving Show

inField :: Field -> Integer -> Bool
inField f i = ( amin f <= i && i <= amax f ) || ( bmin f <= i && i <= bmax f )

readField :: String -> Field
readField line =
	Field (read firstmin) (read firstmax) (read secondmin) (read secondmax)
	where
		_:ranges:_ = splitOn ": " line
		firstRange:secondRange:_ = splitOn " or " ranges
		firstmin:firstmax:_ = splitOn "-" firstRange
		secondmin:secondmax:_ = splitOn "-" secondRange

readFields :: String -> [Field]
readFields = fmap readField . lines

newtype Ticket = Ticket { getFields :: [Integer] } deriving Show

readTicket :: String -> Ticket
readTicket = Ticket . fmap read . splitOn ","

readTickets :: String -> [Ticket]
readTickets = fmap readTicket . tail . lines

errorRate :: [Field] -> Ticket -> Integer
errorRate fields =
	sum . filter (\x -> not $ any (flip inField x) fields) . getFields

readInput :: String -> ([Field], Ticket, [Ticket])
readInput inp = (readFields fields, readTicket yours, readTickets nearby) where
	fields:yourBlock:nearby:_ = splitOn "\n\n" inp
	_:yours:_ = lines yourBlock

totalErrorRate :: ([Field], a, [Ticket]) -> Integer
totalErrorRate (fields, _, tickets) = sum $ errorRate fields <$> tickets

main :: IO ()
main = interact $ show . totalErrorRate . readInput
