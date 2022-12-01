{-# LANGUAGE InstanceSigs #-}

class Example a where
  example :: a

  examples :: [a]
  examples = [example]

instance Example Int where
  example = 1

instance Example Bool where
  example = True

class Size a where
  size :: a -> Int

instance Size [a] where
  size :: [a] -> Int
  size as = length as

class Check a where
  check :: a -> Bool

instance Check Int where
  check n = n > 0

instance Check Bool where
  check b = b

instance Check a => Check [a] where
  check xs = all check xs

class BitOperations a where
  bitNot :: a -> a
  bitNot x = bitNand x x
  bitAnd :: a -> a -> a
  bitAnd x y = bitNot (bitOr (bitNot x) (bitNot y))
  bitOr :: a -> a -> a
  bitOr x y = bitNot (bitAnd (bitNot x) (bitNot y))
  bitNand :: a -> a -> a
  bitNand x y = bitNot (bitAnd x y)

data Test a = T (a -> a) | E (a -> a)