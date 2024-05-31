module Future.Time (
    delaySecs,
    delayMillis,
    delayMicros,
    delayNanos,
) where

import Data.Word

import Future.Duration
import Future.Internal

foreign import ccall "tokio_time_sleep"
  sleep :: Duration -> IO (FuturePtr ())

delay :: IO Duration -> Future ()
delay duration = Future $ sleep =<< duration

delaySecs :: Word64 -> Future ()
delaySecs = delay . fromSecs

delayMillis :: Word64 -> Future ()
delayMillis = delay . fromMillis

delayMicros :: Word64 -> Future ()
delayMicros = delay . fromMicros

delayNanos :: Word64 -> Future ()
delayNanos = delay . fromNanos
