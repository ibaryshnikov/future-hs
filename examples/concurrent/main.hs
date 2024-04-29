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

example :: Future ()
example = do
  liftIO $ putStrLn "Concurrent example"
  let ma = callA 5
  let mb = callB 5
  (a, b) <- concurrent ma mb
  liftIO $ putStrLn $ "(" ++ a ++ ", " ++ show b ++ ")"

main :: IO ()
main = run example
