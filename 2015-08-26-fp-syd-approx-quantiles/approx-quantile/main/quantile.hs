module Main where

import Statistics.Quantile.Bench
import Statistics.Quantile.Exact
import Statistics.Quantile.Util
import Statistics.Quantile.Types

import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  selectFromHandle median external stdin >>= print

