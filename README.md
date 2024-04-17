# future-hs

`Future` Monad for Rust futures in Haskell.
This is a research project which started as part
of [iced-hs](https://github.com/ibaryshnikov/iced-hs)

Built with [tokio](https://github.com/tokio-rs/tokio)

## Description

`Future a` allows to use Rust futures with `do` notation:

```haskell
delay :: Int -> Future () -- Rust Future which waits n seconds

callA :: Future Int
callA = do
  delay 2
  pure 1

callB :: Int -> Future Int
callB a = do
  delay 2
  pure $ a + 4

callFutures :: Future ()
callFutures = do
  a <- callA
  delay 3
  b <- callB a
  delay 1
  liftIO $ putStrLn $ show b -- prints 5
```

It also implements `MonadIO` making it possible to use `IO`:

```haskell
callFutures :: Future ()
callFutures = do
  liftIO $ putStrLn "Starting"
  delay 1
  liftIO $ putStrLn "Tick"
  delay 1
  liftIO $ putStrLn "Done"
```

## Usage

```bash
# build libfuture_hs.a
./build_rust.sh

# call ghc
# note that -threaded is required
ghc -threaded -ipath/to/this/repo path/to/libfuture_hs.a main.hs
```
