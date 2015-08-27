module Main where

import Control.Monad

import Criterion.Main

import Options.Applicative

import Statistics.Quantile.Bench
import Statistics.Quantile.Exact
import Statistics.Quantile.Approximate.Sampling
import Statistics.Quantile.Bench.Gen
import Statistics.Quantile.Types
import Statistics.Quantile.Util

import System.Environment

import System.IO

data Params = Params Bool
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  (fp, fh) <- openTempFile tmpDir "bench"
  void $ countOut fh stream
  hClose fh
  s <- lookupEnv "BENCH_SIZE"
  defaultMain $
    [ benchSelector [benchSize s] "median/exact" median external fp
    , benchBufferingSelector sqrtN [benchSize s] "median/sampling-sqrt(n)" median sampling fp
    , benchBufferingSelector sqrtN [benchSize s] "median/jackknife-sampling-sqrt(n)" median samplingJackknife fp
    ]
  where stream = streamHandle stdin

        benchSize = maybe 10000 read
--        stream = Stream $ gauss 3.14159 10000
