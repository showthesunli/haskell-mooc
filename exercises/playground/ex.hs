data Pair a = P a a deriving (Eq, Ord, Show)

class (Num a, Ord a) => Check a where
  check :: a -> Bool
  check a = a > 0

instance Check Int where
  check n = n > 0

lee :: Int
lee = 1
