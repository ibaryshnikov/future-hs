module Future.Internal where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import System.IO.Unsafe

data NativeFuture a
type FuturePtr a = Ptr (NativeFuture a)

foreign import ccall safe "future_run"
  future_run :: FuturePtr a -> IO ()

data Future a = Future (FuturePtr a)

instance Functor Future where
  fmap = liftM

instance Applicative Future where
  pure = wrap
  (<*>) = ap

instance Monad Future where
  (>>=) = compose

foreign import ccall safe "future_wrap_value"
  wrap_value :: StablePtr a -> FuturePtr a

wrap :: a -> Future a
wrap value = Future $ wrap_value $ makePtr value

makePtr :: a -> StablePtr a
makePtr a = unsafePerformIO $ newStablePtr a

instance MonadIO Future where
  liftIO = wrapIO

foreign import ccall safe "future_wrap_io"
  future_wrap_io :: FunPtr (NativeIO a) -> FuturePtr a

type NativeIO a = IO (StablePtr a)

foreign import ccall "wrapper"
  makeIOCallback :: NativeIO a -> IO (FunPtr (NativeIO a))

foreign export ccall "free_haskell_fun_ptr"
  freeHaskellFunPtr :: FunPtr a -> IO ()

wrapIO :: IO a -> Future a
wrapIO io = Future $ future_wrap_io callback
  where
    callback = unsafePerformIO $ makeIOCallback $ wrapIOtoNative io

wrapIOtoNative :: IO a -> NativeIO a
wrapIOtoNative f = f >>= newStablePtr

foreign import ccall safe "future_compose"
  future_compose :: FuturePtr a -> FunPtr (NativeFAB a b) -> FuturePtr b

type NativeFAB a b = StablePtr a -> FuturePtr b

foreign import ccall "wrapper"
  makeFABCallback :: NativeFAB a b -> IO (FunPtr (NativeFAB a b))

type FAB a b = a -> Future b

wrapFAB :: FAB a b -> NativeFAB a b
wrapFAB fab ptrA = ptrB
  where
    (Future ptrB) = fab a
    a = unsafePerformIO $ fromPtr ptrA

fromPtr :: StablePtr a -> IO (a)
fromPtr ptr = do
  value <- deRefStablePtr ptr
  freeStablePtr ptr
  pure value

compose :: Future a -> FAB a b -> Future b
compose (Future ptrA) fab = Future $ future_compose ptrA nativeFAB
  where
    nativeFAB = unsafePerformIO $ makeFABCallback $ wrapFAB fab

run :: Future a -> IO ()
run (Future ptr) = future_run ptr

foreign import ccall safe "future_concurrent"
  future_concurrent :: FuturePtr a
                    -> FuturePtr b
                    -> FunPtr (NativeTuple a b)
                    -> FuturePtr (a, b)

type NativeTuple a b = StablePtr a -> StablePtr b -> IO (StablePtr (a, b))

foreign import ccall "wrapper"
  makeTuplePtr :: NativeTuple a b -> IO (FunPtr (NativeTuple a b))

tuple :: NativeTuple a b
tuple ptrA ptrB = do
  a <- fromPtr ptrA
  b <- fromPtr ptrB
  newStablePtr (a, b)

concurrent :: Future a -> Future b -> Future (a, b)
concurrent (Future ptrA) (Future ptrB) =
  Future $ future_concurrent ptrA ptrB tuplePtr
  where
    tuplePtr = unsafePerformIO $ makeTuplePtr tuple

foreign import ccall safe "future_race"
  future_race :: FuturePtr a
              -> FuturePtr b
              -> FunPtr (NativeEither a (Either a b))
              -> FunPtr (NativeEither b (Either a b))
              -> FuturePtr (Either a b)

type NativeEither a ma = StablePtr a -> IO (StablePtr ma)

foreign import ccall "wrapper"
  makeEitherPtr :: NativeEither a ma -> IO (FunPtr (NativeEither a ma))

wrapEither :: (a -> ma) -> NativeEither a ma
wrapEither fab ptr = do
  value <- fromPtr ptr
  newStablePtr $ fab value

race :: Future a -> Future b -> Future (Either a b)
race (Future ptrA) (Future ptrB) =
  Future $ future_race ptrA ptrB leftPtr rightPtr
  where
    leftPtr  = unsafePerformIO $ makeEitherPtr $ wrapEither Left
    rightPtr = unsafePerformIO $ makeEitherPtr $ wrapEither Right
