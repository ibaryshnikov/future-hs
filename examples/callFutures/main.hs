module Main where

import Future
import Future.Time

call1 :: Future Int
call1 = do
  delay 2
  pure 10

call2 :: Int -> Future Int
call2 value = do
  delay 3
  pure $ value + 20

callFutures :: Future ()
callFutures = do
  liftIO $ putStrLn "Starting futures"
  delay 1
  liftIO $ putStrLn "tick"
  a <- call1
  liftIO $ putStrLn $ "a is " ++ show a
  b <- call2 a
  liftIO $ do
    putStrLn $ "b is " ++ show b
    putStrLn "Completed"
  pure ()

main :: IO ()
main = do
  run callFutures
