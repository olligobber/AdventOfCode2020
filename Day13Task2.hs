import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Foldable (toList)
import Data.Function (on)
import Control.Monad.Extra (loop)

-- A step of the extended euclidean algorithm, given the two values in mod row
-- and the two values in the accumulator row, returning the result of mod
-- inverse if it exists or the next values for the algorithm
eeaStep :: (Integer, Integer, Integer, Integer) ->
	Either (Integer, Integer, Integer, Integer) (Maybe Integer)
eeaStep (a, b, da, db)
	| b == 1 = Right $ Just db
	| b <= 0 = Right Nothing
	| otherwise = Left (b, a `mod` b, db, -(a `div` b) * db + da)

-- Inverts the first mod the second
modInv :: Integer -> Integer -> Maybe Integer
modInv a b = loop eeaStep (a,b,1,0)

data ArithmeticSequence =
	ArithmeticSequence Integer Integer |
	Empty

instance Semigroup ArithmeticSequence where
	Empty <> _ = Empty
	_ <> Empty = Empty
	ArithmeticSequence a b <> ArithmeticSequence c d
		| (a - c) `mod` gcd d b /= 0 = Empty
		| otherwise = ArithmeticSequence
			(c + d * ((a - c) `div` gcd d b) * y)
			(lcm d b)
		where
			Just y = modInv (d `div` gcd d b) (b `div` gcd d b)

instance Monoid ArithmeticSequence where
	mempty = ArithmeticSequence 0 1

findFirstPositive :: ArithmeticSequence -> Maybe Integer
findFirstPositive Empty = Nothing
findFirstPositive (ArithmeticSequence a b) = Just (a `mod` b)

busToArithmetic :: (Integer, Integer) -> ArithmeticSequence
busToArithmetic (offset, skip) = ArithmeticSequence (-offset) skip

main :: IO ()
main = do
	() <$ getLine
	buses <-
		foldMap toList .
		fmap (traverse readMaybe) .
		zip [0..] .
		splitOn "," <$>
			getLine
		:: IO [(Integer,Integer)]
	print $ findFirstPositive $ foldMap busToArithmetic buses
