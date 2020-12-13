import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Foldable (toList)
import Data.Function (on)

-- Stolen from rosetta code

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

-- End stolen code

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
findFirstPositive (ArithmeticSequence a b) =Just (a `mod` b)

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
	print $ findFirstPositive $ foldMap (busToArithmetic) buses
