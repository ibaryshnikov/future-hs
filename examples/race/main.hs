module Main where

import Future
import Future.Time

callA :: Int -> Future String
callA 0 = pure "Text"
callA n = do
  liftIO $ putStrLn $ "A " ++ show n
  delay 1
  callA $ n - 1

callB :: Int -> Future Int
callB 0 = pure 5
callB n = do
  liftIO $ putStrLn $ "B " ++ show n
  delay 2
  callB $ n - 1

toText :: Show b => Either String b -> String
toText (Left a)  = "A finished first, value is: " ++ a
toText (Right b) = "B finished first, value is: " ++ show b

example :: Future ()
example = do
  liftIO $ putStrLn "Race example"
  let ma = callA 5
  let mb = callB 5
  first <- race ma mb
  liftIO $ putStrLn $ toText first

main :: IO ()
main = run example
