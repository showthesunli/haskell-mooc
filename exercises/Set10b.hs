-- This exercise set hides most of Prelude. You only have access to
-- the Bool, Int and list types, and pattern matching.
--
-- In particular, seq is not available, so you must use pattern
-- matching to force evaluation!
{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.Todo
import Mooc.VeryLimitedPrelude

------------------------------------------------------------------------------
-- Ex 1: Define the operator ||| that works like ||, but forces its
-- _right_ argument instead of the left one.
--
-- Examples:
--   False ||| False     ==> False
--   True ||| False      ==> True
--   undefined ||| True  ==> True
--   False ||| undefined ==> an error!

(|||) :: Bool -> Bool -> Bool
_ ||| True = True
False ||| False = False
True ||| False = True

------------------------------------------------------------------------------
-- Ex 2: Define the function boolLength, that returns the length of a
-- list of booleans and forces all of the elements
--
-- Examples:
--   boolLength [False,True,False] ==> 3
--   boolLength [False,undefined]  ==> an error!
--
-- Note that with the ordinary length function,
--   length [False,undefined] ==> 2

boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (True : xs) = 1 + boolLength xs
boolLength (False : xs) = 1 + boolLength xs

------------------------------------------------------------------------------
-- Ex 3: Define the function validate which, given a predicate and a
-- value, evaluates to the value. However, validate should also force the
-- result of `predicate value`, even though it is not used.
--
-- Examples:
--   validate even 3               ==>  3
--   validate odd 3                ==>  3
--   validate undefined 3          ==>  an error!
--   validate (\x -> undefined) 3  ==>  an error!

validate :: (a -> Bool) -> a -> a
validate predicate value = if predicate value then value else value

------------------------------------------------------------------------------
-- Ex 4: Even though we can't implement the generic seq function
-- ourselves, we can implement it manually for specific datatypes.
--
-- The type class MySeq contains the method myseq which is supposed to
-- work like the built-in seq function. Implement the given MySeq
-- instances.
--
-- Just like in the course material, we use the special value
-- `undefined` here to illustrate what myseq evaluates. The tests for
-- this exercise also use undefined.
--
-- Examples:
--   myseq True  0 ==> 0
--   myseq ((\x -> x) True) 0 ==> 0
--   myseq (undefined :: Bool) 0
--     ==> *** Exception: Prelude.undefined
--   myseq (3::Int) True ==> True
--   myseq (undefined::Int) True
--     ==> *** Exception: Prelude.undefined
--   myseq [1,2] 'z' ==> 'z'
--   myseq [undefined] 'z' ==> 'z'           -- [undefined] is in WHNF
--   myseq (1:undefined) 'z' ==> 'z'         -- 1:undefined is in WHNF
--   myseq (undefined:[2,3]) 'z' ==> 'z'     -- undefined:[2,3] is in WHNF
--   myseq [1..] 'z' ==> 'z'
--   myseq (undefined::[Int])
--     ==> *** Exception: Prelude.undefined

class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq True x = x
  myseq False x = x

instance MySeq Int where
  myseq 1 y = y
  myseq x y = y

instance MySeq [a] where
  myseq [] b = b
  myseq xs b = b
