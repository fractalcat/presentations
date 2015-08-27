module Statistics.Quantile.Types where

import Debug.Trace
import Data.Monoid
import Numeric
import Pipes

data Quantile = Quantile Int Int
  deriving (Eq, Show)

quantile :: Int -> Int -> Quantile
quantile k q
  | q > k     = Quantile k q
  | otherwise = error "quantile not defined where k >= q"

qidx :: Quantile -> Int -> Int
qidx (Quantile k q) n =
  let i = (fromIntegral (n * k)) / (fromIntegral q)
  in floor i

type Population = Producer Double IO ()

newtype Stream m = Stream (Producer Double m ())

newtype TrueValue = TrueValue { unTrueValue :: Double }
  deriving (Eq, Show)

newtype Estimate = Estimate { unEstimate :: Double }
  deriving (Eq, Show)

data Deviation = Deviation Double Double
  deriving (Eq, Show, Ord)

renderDeviation :: Deviation -> String
renderDeviation (Deviation mse sigma) = concat $
  [ decimalFloat mse
  , " MSE ("
  , "stddev "
  , decimalFloat sigma
  , ")"
  ]

decimalFloat v = showFFloat Nothing v ""

data QuantileValue = TrueQuantile TrueValue
                   | QuantileEstimate Estimate
  deriving (Eq, Show)

newtype Selector m =
  Selector
    { unSelector ::
     (   Quantile
      -> Stream m
      -> m Double
     )
    }

data QuantileError = EmptySample
  deriving (Eq, Show)

median = quantile 1 2

percentile p = quantile p 100
