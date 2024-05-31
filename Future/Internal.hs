module Future.Internal where

import Control.Monad
import Control.Monad.IO.Class
import Foreign

data NativeFuture a
type FuturePtr a = Ptr (NativeFuture a)

newtype Future a = Future (IO (FuturePtr a))

instance Monad Future where
  (>>=) = compose

instance Functor Future where
  fmap = liftM

instance Applicative Future where
  pure = wrap
  -- (*>) = sequential -- is that useful?
  (<*>) = ap

instance MonadIO Future where
  liftIO = wrapIO

foreign export ccall "free_haskell_fun_ptr"
  freeHaskellFunPtr :: FunPtr a -> IO ()

-- wrap STARTS

foreign import ccall "future_wrap_value"
  wrap_value :: StablePtr a -> IO (FuturePtr a)

wrap :: a -> Future a
wrap a = Future $ wrap_value =<< newStablePtr a

wrapIO :: IO a -> Future a
wrapIO io = Future $ wrap_value =<< newStablePtr =<< io

-- wrap ENDS

-- sequential STARTS

foreign import ccall "future_sequential"
  future_sequential :: FuturePtr a -> FuturePtr b -> IO (FuturePtr b)

-- potentially useful, but it's ignored by the compiler
-- even if you declare (*>) = sequential
sequential :: Future a -> Future b -> Future b
sequential (Future ma) (Future mb) = Future $ do
  ptrA <- ma
  ptrB <- mb
  future_sequential ptrA ptrB

-- sequential ENDS

-- compose STARTS

foreign import ccall "future_compose"
  future_compose :: FuturePtr a -> FunPtr (NativeFAB a b) -> IO (FuturePtr b)

type NativeFAB a b = StablePtr a -> IO (FuturePtr b)

foreign import ccall "wrapper"
  makeFABCallback :: NativeFAB a b -> IO (FunPtr (NativeFAB a b))

wrapFAB :: (a -> Future b) -> NativeFAB a b
wrapFAB fab ptrA = do
  a <- fromPtr ptrA
  case fab a of (Future mb) -> mb

compose :: Future a -> (a -> Future b) -> Future b
compose (Future ma) fab = Future $ do
  ptrA <- ma
  fabPtr <- makeFABCallback $ wrapFAB fab
  future_compose ptrA fabPtr

freePtr :: StablePtr a -> IO ()
freePtr ptr = case ptrToIntPtr $ castStablePtrToPtr ptr of
  -- from the docs:
  -- "The StablePtr 0 is reserved for representing NULL in foreign code."
  0 -> pure ()
  _ -> freeStablePtr ptr

fromPtr :: StablePtr a -> IO (a)
fromPtr ptr = do
  value <- deRefStablePtr ptr
  freePtr ptr
  pure value

-- compose ENDS

-- spawnBlocking STARTS

foreign import ccall "future_spawn_blocking"
  spawn_blocking :: FunPtr (IO (StablePtr a)) -> IO (FuturePtr a)

foreign import ccall "wrapper"
  makeBlockingCallback :: IO (StablePtr a) -> IO (FunPtr (IO (StablePtr a)))

wrapBlockingCallback :: IO a -> IO (StablePtr a)
wrapBlockingCallback io = newStablePtr =<< io

spawnBlocking :: IO a -> Future a
spawnBlocking io = Future $ do
  let callback = wrapBlockingCallback io
  spawn_blocking =<< makeBlockingCallback callback

-- spawnBlocking ENDS

-- run STARTS

foreign import ccall "future_run"
  future_run :: FunPtr MainCallback -> IO ()

type MainCallback = IO (FuturePtr ())

foreign import ccall "wrapper"
  makeMainCallback :: MainCallback -> IO (FunPtr MainCallback)

run :: (Future ()) -> IO ()
run (Future callback) = future_run =<< makeMainCallback callback

-- run ENDS

-- concurrent STARTS

foreign import ccall "future_concurrent"
  future_concurrent :: FuturePtr a -> FuturePtr b -> IO (FuturePtr (a, b))

type NativePair a b = StablePtr a -> StablePtr b -> IO (StablePtr (a, b))

foreign export ccall "future_pair"
  pair :: NativePair a b

pair :: NativePair a b
pair ptrA ptrB = do
  a <- fromPtr ptrA
  b <- fromPtr ptrB
  newStablePtr (a, b)

concurrent :: Future a -> Future b -> Future (a, b)
concurrent (Future ma) (Future mb) = Future $ do
  ptrA <- ma
  ptrB <- mb
  future_concurrent ptrA ptrB

-- concurrent ENDS

-- race STARTS

foreign import ccall "future_race"
  future_race :: FuturePtr a -> FuturePtr b -> IO (FuturePtr (Either a b))

type NativeEither a ma = StablePtr a -> IO (StablePtr ma)

foreign export ccall "future_left"  left  :: NativeEither a (Either a b)
foreign export ccall "future_right" right :: NativeEither b (Either a b)

left :: NativeEither a (Either a b)
left = makeEither Left

right :: NativeEither b (Either a b)
right = makeEither Right

makeEither :: (a -> ma) -> NativeEither a ma
makeEither f ptr = do
  a <- fromPtr ptr
  newStablePtr $ f a

race :: Future a -> Future b -> Future (Either a b)
race (Future ma) (Future mb) = Future $ do
  ptrA <- ma
  ptrB <- mb
  future_race ptrA ptrB

-- race ENDS
