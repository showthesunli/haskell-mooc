getNum :: Maybe a -> Maybe b -> b
getNum x y = do
  x' <- x
  y' <- y
  return y