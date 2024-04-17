module Future.Time (
    delay,
) where

import Foreign
import Foreign.C.Types

import Future
import Future.Internal

data NativeDuration
type DurationPtr = Ptr NativeDuration

foreign import ccall safe "duration_from_secs"
  duration_from_secs :: CULong -> DurationPtr

foreign import ccall safe "tokio_time_sleep"
  tokio_time_sleep :: DurationPtr -> FuturePtr ()

delay :: Int -> Future ()
delay seconds = Future $ tokio_time_sleep duration
  where value = CULong $ fromIntegral seconds
        duration = duration_from_secs value
