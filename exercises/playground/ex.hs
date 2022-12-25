import Control.Concurrent (forkIO)

printA :: IO ()
printA = do
  putStrLn $ replicate 40 'A'

printB :: IO ()
printB = do
  putStrLn $ replicate 40 'B'

concurrency :: IO ()
concurrency = do
  forkIO printA
  forkIO printB
  return ()