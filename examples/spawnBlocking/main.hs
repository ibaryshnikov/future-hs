module Main where

import Control.Concurrent

import Future

seconds :: Int -> Int
seconds = (* 1_000_000)

callA :: Int -> IO String
callA 0 = pure "Text"
callA n = do
  liftIO $ putStrLn $ "A " ++ show n
  threadDelay $ seconds 1
  callA $ n - 1

callB :: Int -> IO Int
callB 0 = pure 5
callB n = do
  liftIO $ putStrLn $ "B " ++ show n
  threadDelay $ seconds 2
  callB $ n - 1

blocked :: Future ()
blocked = do
  liftIO $ putStrLn "Runtime is blocked by threadDelay"
  let ma = liftIO $ callA 5
  let mb = liftIO $ callB 5
  (a, b) <- concurrent ma mb
  liftIO $ putStrLn $ "(" ++ a ++ ", " ++ show b ++ ")"

spawned :: Future ()
spawned = do
  liftIO $ putStrLn "Blocking threadDelay calls are moved to another thread"
  let ma = spawnBlocking $ callA 5
  let mb = spawnBlocking $ callB 5
  (a, b) <- concurrent ma mb
  liftIO $ putStrLn $ "(" ++ a ++ ", " ++ show b ++ ")"

example :: Future ()
example = do
  liftIO $ putStrLn "Spawn blocking example"
  blocked
  spawned

main :: IO ()
main = run example
