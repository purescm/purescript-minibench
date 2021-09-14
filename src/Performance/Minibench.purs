-- | This module provides the `bench` function, which prints a short summary
-- | of the running times of a synchronous function to the console.
-- |
-- | For benchmarking tasks which require finer accuracy, or graphs as output,
-- | consider using `purescript-benchotron` instead.

module Performance.Minibench
  ( bench
  , benchWith
  , benchWith'
  , BenchResult
  , withUnits
  ) where

import Prelude hiding (min,max)

import Data.Int (toNumber)
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Ref as Ref
import Data.Number (infinity)
import Math (max, min, sqrt)
import Partial.Unsafe (unsafeCrashWith)

foreign import time :: Effect (Array Int)

-- | Force garbage collection.
-- | Requires node to be run with the --force-gc flag.
foreign import gc :: Effect Unit

foreign import toFixed :: Number -> String

fromTime :: Array Int -> Number
fromTime [s, ns] = toNumber s * 1.0e9 + toNumber ns
fromTime _ = unsafeCrashWith "fromHrTime: unexpected result from process.hrtime()"

timeDiff :: Array Int -> Array Int -> Array Int
timeDiff [t1s, t1ns] [t2s, t2ns] = [t2s - t1s, t2ns - t1ns]
timeDiff _ _ = unsafeCrashWith "timeDiff: unexpected result"

withUnits :: Number -> String
withUnits t
  | t < 1.0e3 = toFixed t <> " ns"
  | t < 1.0e6 = toFixed (t / 1.0e3) <> " us"
  | t < 1.0e9 = toFixed (t / 1.0e6) <> " ms"
  | otherwise = toFixed (t / 1.0e9) <> " s"

-- | Estimate the running time of a function and print a summary to the console,
-- | specifying the number of samples to take. More samples will give a better
-- | estimate of both mean and standard deviation, but will increase running time.
-- |
-- | To increase benchmark accuracy by forcing garbage collection before the
-- | benchmark is run, node should be invoked with the '--expose-gc' flag.
benchWith
  :: forall a
   . Int
  -> (Unit -> a)
  -> Effect Unit
benchWith n f = do
  res <- benchWith' n f
  log ("mean   = " <> withUnits res.mean)
  log ("stddev = " <> withUnits res.stdDev)
  log ("min    = " <> withUnits res.min)
  log ("max    = " <> withUnits res.max)

type BenchResult =
  { mean :: Number
  , stdDev :: Number
  , min :: Number
  , max :: Number
  }

benchWith'
  :: forall a
   . Int
  -> (Unit -> a)
  -> Effect BenchResult
benchWith' n f = do
  sumRef <- Ref.new 0.0
  sum2Ref <- Ref.new 0.0
  minRef <- Ref.new infinity
  maxRef <- Ref.new 0.0
  gc
  forE 0 n \_ -> do
    t1 <- time
    t2 <- const time (f unit)
    let t      = timeDiff t1 t2
        ns     = fromTime t
        square = ns * ns
    _ <- Ref.modify (_ + ns) sumRef
    _ <- Ref.modify (_ + square) sum2Ref
    _ <- Ref.modify (_ `min` ns) minRef
    _ <- Ref.modify (_ `max` ns) maxRef
    pure unit
  sum <- Ref.read sumRef
  sum2 <- Ref.read sum2Ref
  min' <- Ref.read minRef
  max' <- Ref.read maxRef
  let n'     = toNumber n
      mean   = sum / n'
      stdDev = sqrt ((sum2 - n' * mean * mean) / (n' - 1.0))
  pure
    { mean
    , stdDev
    , min: min'
    , max: max'
    }

-- | Estimate the running time of a function and print a summary to the console,
-- | by running the function 1000 times.
-- |
-- | For example:
-- |
-- | ```
-- | > import Data.Array
-- | > import Data.Foldable
-- | > import Performance.Minibench
-- | > bench \_ -> sum (1 .. 10000)
-- |
-- | mean   = 414.00 μs
-- | stddev = 494.82 μs
-- | ```
bench :: forall a. (Unit -> a) -> Effect Unit
bench = benchWith 1000
