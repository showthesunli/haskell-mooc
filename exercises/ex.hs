import Control.Monad
import Network.HTTP

main :: IO ()
main = do
  rsp <- simpleHTTP (getRequest "http://httpbin.org/base64/aGFza2VsbCBmb3IgZXZlcgo=")
  body <- getResponseBody rsp
  forM_ (words body) $ \w -> do
    putStr "word: "
    putStrLn w
