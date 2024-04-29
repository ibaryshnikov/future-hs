module Future.Time (
    delay,
) where

import Control.Monad
import Data.Word
import Foreign
import Foreign.C.Types

import Future.Internal

data NativeDuration
type DurationPtr = Ptr NativeDuration

foreign import ccall "duration_from_secs"
  duration_from_secs :: CULong -> IO (DurationPtr)

foreign import ccall "tokio_time_sleep"
  tokio_time_sleep :: DurationPtr -> IO (FuturePtr ())

makeDelay :: Word64 -> IO (FuturePtr ())
makeDelay = tokio_time_sleep <=< duration_from_secs . CULong

delay :: Word64 -> Future ()
delay n = Future (makeDelay n)
