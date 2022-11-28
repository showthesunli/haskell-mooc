import Data.List

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST Int -> State -> (Int, State)
app (S st) = st

instance Functor ST where
  fmap g (S st) =
    S
      ( \s ->
          let (n, s') = st s
           in (g n, s')
      )
