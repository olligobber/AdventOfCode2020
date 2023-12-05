import Data.List.Split (splitOn)

data Field = Field {
	name :: String,
	amin :: Integer,
	amax :: Integer,
	bmin :: Integer,
	bmax :: Integer
	} deriving Show

inField :: Field -> Integer -> Bool
inField f i = ( amin f <= i && i <= amax f ) || ( bmin f <= i && i <= bmax f )

readField :: String -> Field
readField line = Field
	fieldName
	(read firstmin)
	(read firstmax)
	(read secondmin)
	(read secondmax)
	where
		fieldName:ranges:_ = splitOn ": " line
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

-- Check if a ticket scanned correctly as every field fits some requirement
isValid :: [Field] -> Ticket -> Bool
isValid fields = all (\x -> any (flip inField x) fields) . getFields

readInput :: String -> ([Field], Ticket, [Ticket])
readInput inp = (readFields fields, readTicket yours, readTickets nearby) where
	fields:yourBlock:nearby:_ = splitOn "\n\n" inp
	_:yours:_ = lines yourBlock
