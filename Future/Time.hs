module Future.Time (
    delay,
) where

import Control.Monad
import GHC.Base
import Foreign
import Foreign.C.Types

import Future.Internal

data NativeDuration
type DurationPtr = Ptr NativeDuration

foreign import ccall safe "duration_from_secs"
  duration_from_secs :: CULong -> IO (DurationPtr)

foreign import ccall safe "tokio_time_sleep"
  tokio_time_sleep :: DurationPtr -> IO (FuturePtr ())

makeDelay :: Int -> IO (FuturePtr ())
makeDelay = tokio_time_sleep <=< duration_from_secs . fromIntegral

delay :: Int -> Future ()
delay n = Future (\s -> unIO (makeDelay n) s)
