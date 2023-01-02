newtype State s a = S (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (S f) = f

put :: a -> State s a
put x = S f
  where
    f y = (x, y)

get :: State s s
get = S g
  where
    g s = (s, s)

-- operation that modifies the current state with a function (and produce ())
modify :: (s -> s) -> State s ()
modify g = S (\s -> ((), g s))

instance Functor (State s) where
  fmap g state = S f
    where
      f s =
        let (res, s') = runState state s
            res' = g res
         in (res', s')

instance Applicative (State s) where
  pure = put

  (<*>) statef state =
    S
      ( \s ->
          let (f, s') = runState statef s
              (res, s'') = runState state s
           in (f res, s'')
      )

instance Monad (State s) where
  return = pure

  (>>=) state g =
    S
      ( \s ->
          let (res, s') = runState state s
              op1 = g res
           in runState op1 s'
      )

add1 :: Num a => State a a
add1 = (1 +) <$> get

add2 :: Num a => State a a
add2 = do
  init <- get
  modify (+ 2)
  modify (+ 12)
  put 1000

main :: IO ()
main = do
  let (res, s) = runState add2 1
  print res
  print s
