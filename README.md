# future-hs

`Future` Monad for Rust futures in Haskell.
This is a research project which started as part
of [iced-hs](https://github.com/ibaryshnikov/iced-hs)

Built with [tokio](https://github.com/tokio-rs/tokio)


## Example

```haskell
import Future
import Future.Time

callA :: Future Int
callA = do
  delay 2
  pure 1

callB :: Int -> Future Int
callB a = do
  delay 2
  pure $ a + 4

example :: Future ()
example = do
  a <- callA
  b <- callB a
  liftIO $ putStrLn $ show b -- prints 5

main :: IO ()
main = run example
```

There are `race` and `concurrent` functions:

```haskell
race :: Future a -> Future b -> Future (Either a b)
concurrent :: Future a -> Future b -> Future (a, b)
```

For example:

```haskell
do
  let ma = callA
  let mb = callB
  (a, b) <- concurrent ma mb
```

`race` is a wrapper around
[tokio::select!](https://docs.rs/tokio/1.37.0/tokio/macro.select.html)
and `concurrent` uses
[tokio::join!](https://docs.rs/tokio/1.37.0/tokio/macro.join.html)


## Usage

```bash
# build libfuture_hs.a
./build_rust.sh

# call ghc
# note that -threaded is required
ghc -threaded -ipath/to/this/repo path/to/libfuture_hs.a main.hs
```
