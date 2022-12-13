getNum :: Maybe a -> IO a
getNum x = x >>= (\cur -> return cur)
