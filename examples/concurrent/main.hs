module Main where

import Future
import Future.Time

callA :: Int -> Future String
callA 0 = pure "Text"
callA n = do
  delay 1
  liftIO $ putStrLn $ "A " ++ show n
  callA $ n - 1

callB :: Int -> Future Int
callB 0 = pure 5
callB n = do
  delay 2
  liftIO $ putStrLn $ "B " ++ show n
  callB $ n - 1

example :: Future ()
example = do
  liftIO $ putStrLn "Concurrent example"
  let ma = callA 5
  let mb = callB 5
  (a, b) <- concurrent ma mb
  liftIO $ putStrLn $ "(" ++ a ++ ", " ++ show b ++ ")"
  pure ()

main :: IO ()
main = run example
