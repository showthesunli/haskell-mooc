-- Exercise set 6: defining classes and instances

module Set6 where

import Data.Char (toLower)
import Mooc.LimitedPrelude (Maybe (Just))
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: define an Eq instance for the type Country below. You'll need
-- to use pattern matching.

data Country = Finland | Switzerland | Norway
  deriving (Show)

instance Eq Country where
  Finland == Finland = True
  Switzerland == Switzerland = True
  Norway == Norway = True
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 2: implement an Ord instance for Country so that
--   Finland <= Norway <= Switzerland
--
-- Remember minimal complete definitions!

instance Ord Country where
  compare x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT
  Finland <= _ = True -- and me?
  _ <= Switzerland = True
  x <= y = x == y

  min x y = if x <= y then x else y
  max x y = if x == min x y then y else x -- and me?

------------------------------------------------------------------------------
-- Ex 3: Implement an Eq instance for the type Name which contains a String.
-- The Eq instance should ignore capitalization.
--
-- Hint: use the function Data.Char.toLower that has been imported for you.
--
-- Examples:
--   Name "Pekka" == Name "pekka"   ==> True
--   Name "Pekka!" == Name "pekka"  ==> False

data Name = Name String
  deriving (Show)

instance Eq Name where
  (==) (Name x) (Name y) = length x == length y && and [toLower nx == toLower ny | (nx, ny) <- zip x y]

------------------------------------------------------------------------------
-- Ex 4: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq (List a)" that compares the lists element
-- by element.
--
-- Note how the instance needs an Eq a constraint. What happens if you
-- remove it?

data List a = Empty | LNode a (List a)
  deriving (Show)

instance Eq a => Eq (List a) where
  (==) Empty Empty = True
  (==) (LNode x nx) (LNode y ny) = x == y && (nx == ny)
  (==) _ _ = False

------------------------------------------------------------------------------
-- Ex 5: below you'll find two datatypes, Egg and Milk. Implement a
-- type class Price, containing a function price. The price function
-- should return the price of an item.
--
-- The prices should be as follows:

-- * chicken eggs cost 20

-- * chocolate eggs cost 30

-- * milk costs 15 per liter

--
-- Example:
--   price ChickenEgg  ==>  20

data Egg = ChickenEgg | ChocolateEgg
  deriving (Show)

data Milk = Milk Int -- amount in litres
  deriving (Show)

class Price a where
  price :: a -> Int

instance Price Egg where
  price ChickenEgg = 20
  price ChocolateEgg = 30

instance Price Milk where
  price (Milk n) = 15 * n

------------------------------------------------------------------------------
-- Ex 6: define the necessary instance hierarchy in order to be able
-- to compute these:
--
-- price (Just ChickenEgg) ==> 20
-- price [Milk 1, Milk 2]  ==> 45
-- price [Just ChocolateEgg, Nothing, Just ChickenEgg]  ==> 50
-- price [Nothing, Nothing, Just (Milk 1), Just (Milk 2)]  ==> 45
instance Price a => Price (Maybe a) where
  price Nothing = 0
  price (Just x) = price x

instance Price a => Price [a] where
  price xs = foldr (\cur rest -> price cur + rest) 0 xs

------------------------------------------------------------------------------
-- Ex 7: below you'll find the datatype Number, which is either an
-- Integer, or a special value Infinite.
--
-- Implement an Ord instance so that finite Numbers compare normally,
-- and Infinite is greater than any other value.

data Number = Finite Integer | Infinite
  deriving (Show, Eq)

instance Ord Number where
  _ <= Infinite = True
  Finite x <= Finite y = x <= y
  x <= y = x == y

------------------------------------------------------------------------------
-- Ex 8: rational numbers have a numerator and a denominator that are
-- integers, usually separated by a horizontal bar or a slash:
--
--      numerator
--    -------------  ==  numerator / denominator
--     denominator
--
-- You may remember from school that two rationals a/b and c/d are
-- equal when a*d == b*c. Implement the Eq instance for rationals
-- using this definition.
--
-- You may assume in all exercises that the denominator is always
-- positive and nonzero.
--
-- Examples:
--   RationalNumber 4 5 == RationalNumber 4 5    ==> True
--   RationalNumber 12 15 == RationalNumber 4 5  ==> True
--   RationalNumber 13 15 == RationalNumber 4 5  ==> False

data RationalNumber = RationalNumber Integer Integer
  deriving (Show)

instance Eq RationalNumber where
  RationalNumber 0 _ == RationalNumber 0 _ = True
  RationalNumber x y == RationalNumber n m = x * m == y * n

------------------------------------------------------------------------------
-- Ex 9: implement the function simplify, which simplifies rational a
-- number by removing common factors of the numerator and denominator.
-- In other words,
--
--     ca         a
--    ----  ==>  ---
--     cb         b
--
-- As a concrete example,
--
--     12        3 * 4         4
--    ----  ==  -------  ==>  ---.
--     15        3 * 5         5
--
-- Hint: Remember the function gcd?

simplify :: RationalNumber -> RationalNumber
-- simplify (RationalNumber 0 x) = RationalNumber 0 x
simplify (RationalNumber x y) = RationalNumber (x `div` gcd') (y `div` gcd')
  where
    gcd' = gcd x y

------------------------------------------------------------------------------
-- Ex 10: implement the typeclass Num for RationalNumber. The results
-- of addition and multiplication must be simplified.
--
-- Reminders:
--   * negate x is 0-x
--   * abs is absolute value
--   * signum is -1, +1 or 0 depending on the sign of the input
--
-- Examples:
--   RationalNumber 1 3 + RationalNumber 1 6 ==> RationalNumber 1 2
--   RationalNumber 1 3 * RationalNumber 3 1 ==> RationalNumber 1 1
--   negate (RationalNumber 2 3)             ==> RationalNumber (-2) 3
--   fromInteger 17 :: RationalNumber        ==> RationalNumber 17 1
--   abs (RationalNumber (-3) 2)             ==> RationalNumber 3 2
--   signum (RationalNumber (-3) 2)          ==> RationalNumber (-1) 1
--   signum (RationalNumber 0 2)             ==> RationalNumber 0 1

instance Num RationalNumber where
  RationalNumber x y + RationalNumber m n
    | x == 0 && m == 0 = RationalNumber 0 1
    | x == 0 = simplify $ RationalNumber m n
    | m == 0 = simplify $ RationalNumber x y
    | otherwise = simplify (RationalNumber ((x * n) + (y * m)) (y * n))
  RationalNumber x y * RationalNumber m n = simplify $ RationalNumber (x * m) (y * n)
  abs (RationalNumber x y) = RationalNumber (abs x) y
  signum (RationalNumber x y)
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0
  fromInteger x = RationalNumber x 1
  negate (RationalNumber x y) = RationalNumber (- x) y

------------------------------------------------------------------------------
-- Ex 11: a class for adding things. Define a class Addable with a
-- constant `zero` and a function `add`. Define instances of Addable
-- for Integers and lists. Numbers are added with the usual addition,
-- while lists are added by catenating them. Pick a value for `zero`
-- such that: `add zero x == x`
--
-- Examples:
--   add 1 2                ==>  3
--   add 1 zero             ==>  1
--   add [1,2] [3,4]        ==>  [1,2,3,4]
--   add zero [True,False]  ==>  [True,False]
class Addable a where
  zero :: a
  add :: a -> a -> a

instance Addable Integer where
  add x y = x + y
  zero = 0

instance Addable [a] where
  add x y = x ++ y
  zero = []

------------------------------------------------------------------------------
-- Ex 12: cycling. Implement a type class Cycle that contains a
-- function `step` that cycles through the values of the type.
-- Implement instances for Color and Suit that work like this:
--
--   step Red ==> Green
--   step Green ==> Blue
--   step Blue ==> Red
--
-- The suit instance should cycle suits in the order Club, Spade,
-- Diamond, Heart, Club.
--
-- Also add a function `stepMany` to the class and give it a default
-- implementation using `step`. The function `stepMany` should take
-- multiple (determined by an Int argument) steps like this:
--
--   stepMany 2 Club ==> Diamond
--   stepMany 3 Diamond ==> Spade
--
-- The tests will test the Cycle class and your default implementation
-- of stepMany by adding an instance like this:
--
--    instance Cycle Int where
--      step = succ

data Color = Red | Green | Blue
  deriving (Show, Eq)

data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq)

class Cycle a where
  step :: a -> a

  stepMany :: Int -> a -> a
  stepMany 0 x = x
  stepMany n x = stepMany (n -1) (step x)

infi :: [a] -> [a]
infi xs = xs ++ infi xs

oc :: [Color]
oc = infi [Red, Green, Blue]

os :: [Suit]
os = infi [Club, Spade, Diamond, Heart]

instance Cycle Color where
  step a = next oc
    where
      next (c : cs) = if c == a then head cs else next cs
      next [] = Red

instance Cycle Suit where
  step a = next os
    where
      next (c : cs) = if c == a then head cs else next cs
      next [] = Club