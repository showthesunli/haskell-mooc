import Control.Monad

getPair :: Eq a => a -> [(a, b)] -> Maybe (a, b)
getPair target xs = if null res then Nothing else Just (head res)
  where
    res = do
      (p, x) <- xs
      if p == target then [(p, x)] else []

ender = getPair "ender" [("ender", 13), ("orson", 6), ("scott", 5)]

no = getPair "no" [("ender", 13), ("orson", 6), ("scott", 5)]