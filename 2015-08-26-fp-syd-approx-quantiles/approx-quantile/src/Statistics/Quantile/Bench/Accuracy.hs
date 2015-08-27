module Statistics.Quantile.Bench.Accuracy where

import qualified Data.Vector as V

import Statistics.Quantile.Types
import Statistics.Quantile.Util

import Statistics.Sample

import System.IO

err :: Double
    -> Double
    -> Double
err true estimate = 
  let e = abs (true - estimate)
  in e * e

selectorAccuracy :: Stream IO
                 -> Quantile
                 -> Double
                 -> Selector IO
                 -> IO Double
selectorAccuracy src q true (Selector select) =
  err true <$> select q src

benchAccuracy :: Int
              -> FilePath
              -> Quantile
              -> Double
              -> Selector IO
              -> IO Deviation
benchAccuracy n fp q true s = do
  as <- V.replicateM n accuracy
  pure $ Deviation (mean as) (stdDev as)
  where accuracy = do
          h <- openFile fp ReadMode
          r <- selectorAccuracy (streamHandle h) q true s
          hClose h
          pure r
