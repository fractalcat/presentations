{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid

import qualified Pipes.Prelude as P

import Statistics.Quantile.Util
import Statistics.Quantile.Exact
import Statistics.Quantile.Approximate.Sampling
import Statistics.Quantile.Bench
import Statistics.Quantile.Bench.Accuracy
import Statistics.Quantile.Types

import System.IO

data SelectorAccuracy = SelectorAccuracy String Deviation
  deriving (Eq, Show)

renderAccuracy :: SelectorAccuracy -> String
renderAccuracy (SelectorAccuracy s d) = s <> "," <> renderDeviation d

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  (fp, fh) <- openTempFile tmpDir "accuracy.txt"
  n <- countOut fh (streamHandle stdin)
  hClose fh
  inh <- openFile fp ReadMode
  tv <- selectFromHandle median external inh
  hClose inh
  mapM (score fp tv) (estimators n sqrtN)
    >>= mapM_ (putStrLn . renderAccuracy)
  where estimators n f = [ ("exact", external)
                         , ("sampling", sampling (f n) n)
                         , ("jackknife-sampling", samplingJackknife (f n) n)
                         ]

        score fp tv (name, candidate) = do
          a <- benchAccuracy 10 fp median tv candidate
          pure (SelectorAccuracy name a)
