substrings :: String -> [String]
substrings xs = do
  start <- [0 .. length xs - 1]
  end <- [start + 1 .. length xs - 1]
  return $ drop start $ take end $ xs

getSub :: [a] -> [[a]]
getSub [] = []
getSub (x : xs) = [x] : foldr f [] (getSub xs)
  where
    f ys r = ys : (x : ys) : r