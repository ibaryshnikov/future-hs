{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Future.Internal where

import Control.Monad
import Control.Monad.IO.Class
import GHC.Base
import Foreign

data NativeFuture a
type FuturePtr a = Ptr (NativeFuture a)

newtype Future a = Future (State# RealWorld -> (# State# RealWorld, FuturePtr a #))

unFuture :: Future a -> (State# RealWorld -> (# State# RealWorld, FuturePtr a #))
unFuture (Future a) = a

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

foreign import ccall safe "future_wrap_value"
  wrap_value :: StablePtr a -> IO (FuturePtr a)

wrapValue :: a -> IO (FuturePtr a)
wrapValue = wrap_value <=< newStablePtr

wrap :: a -> Future a
wrap a = Future (\s -> unIO (wrapValue a) s)

-- wrap ENDS

-- sequential STARTS

foreign import ccall safe "future_sequential"
  future_sequential :: FuturePtr a -> FuturePtr b -> IO (FuturePtr b)

futureSequential :: FuturePtr a -> Future b -> IO (FuturePtr b)
futureSequential ptrA (Future mb) = IO (\s -> case mb s of
  (# new_s, ptrB #) -> unIO (future_sequential ptrA ptrB) new_s)

-- potentially useful, but it's ignored by the compiler
-- even if you declare (*>) = sequential
sequential :: Future a -> Future b -> Future b
sequential (Future ma) mb = Future (\s -> case ma s of
  (# new_s, ptrA #) -> unIO (futureSequential ptrA mb) new_s)

-- sequential ENDS

-- compose STARTS

foreign import ccall safe "future_compose"
  future_compose :: FuturePtr a -> FunPtr (NativeFAB a b) -> IO (FuturePtr b)

type NativeFAB a b = StablePtr a -> IO (FuturePtr b)

foreign import ccall "wrapper"
  makeFABCallback :: NativeFAB a b -> IO (FunPtr (NativeFAB a b))

wrapFAB :: (a -> Future b) -> NativeFAB a b
wrapFAB fab ptrA = IO (\s -> case unIO (fromPtr ptrA) s of
  (# new_s, a #) -> unFuture (fab a) new_s)

futureCompose :: FuturePtr a -> (a -> Future b) -> IO (FuturePtr b)
futureCompose ptrA = future_compose ptrA <=< makeFABCallback . wrapFAB

compose :: Future a -> (a -> Future b) -> Future b
compose (Future m) fab = Future (\s -> case m s of
  (# new_s, ptrA #) -> unIO (futureCompose ptrA fab) new_s)

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

-- wrapIO STARTS

foreign import ccall safe "future_wrap_io"
  future_wrap_io :: FunPtr (IO (StablePtr a)) -> IO (FuturePtr a)

foreign import ccall "wrapper"
  makeIOCallback :: IO (StablePtr a) -> IO (FunPtr (IO (StablePtr a)))

passIO :: IO a -> IO (FuturePtr a)
passIO = future_wrap_io <=< makeIOCallback . (=<<) newStablePtr

wrapIO :: IO a -> Future a
wrapIO io = Future (\s -> unIO (passIO io) s)

-- wrapIO ENDS

-- run STARTS

foreign import ccall safe "future_run"
  future_run :: FunPtr MainCallback -> IO ()

type MainCallback = IO (FuturePtr ())

foreign import ccall "wrapper"
  makeMainCallback :: MainCallback -> IO (FunPtr MainCallback)

wrapMain :: Future () -> IO (FuturePtr ())
wrapMain m = IO (\s -> unFuture m s)

run :: (Future ()) -> IO ()
run = future_run <=< makeMainCallback . wrapMain

-- run ENDS

-- concurrent STARTS

foreign import ccall safe "future_concurrent"
  future_concurrent :: FuturePtr a -> FuturePtr b -> IO (FuturePtr (a, b))

type NativePair a b = StablePtr a -> StablePtr b -> IO (StablePtr (a, b))

foreign export ccall "future_pair"
  pair :: NativePair a b

pair :: NativePair a b
pair ptrA ptrB = do
  a <- fromPtr ptrA
  b <- fromPtr ptrB
  newStablePtr (a, b)

futureConcurrent :: FuturePtr a -> Future b -> IO (FuturePtr (a, b))
futureConcurrent ptrA (Future mb) = IO(\s -> case mb s of
  (# new_s, ptrB #) -> unIO (future_concurrent ptrA ptrB) new_s)

concurrent :: Future a -> Future b -> Future (a, b)
concurrent (Future ma) mb = Future (\s -> case ma s of
  (# new_s, ptrA #) -> unIO (futureConcurrent ptrA mb) new_s)

-- concurrent ENDS

-- race STARTS

foreign import ccall safe "future_race"
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

futureRace :: FuturePtr a -> Future b -> IO (FuturePtr (Either a b))
futureRace ptrA (Future mb) = IO (\s -> case mb s of
  (# new_s, ptrB #) -> unIO (future_race ptrA ptrB) new_s)

race :: Future a -> Future b -> Future (Either a b)
race (Future ma) mb = Future (\s -> case ma s of
  (# new_s, ptrA #) -> unIO (futureRace ptrA mb) new_s)

-- race ENDS
