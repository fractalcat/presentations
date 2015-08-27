module Statistics.Quantile.Bench.Gen(
    gauss
  , gamma
  , sampleGaussian
  , sampleGamma
) where

import Control.Applicative
import Control.Monad

import Data.Random (runRVar)
import Data.Random.Distribution
import Data.Random.Distribution.Normal
import qualified Data.Random.Distribution.Gamma as G

import Pipes

import Statistics.Quantile.Types

import System.Random.MWC

gauss :: Double -> Double -> Producer Double IO ()
gauss = runSampler sampleGaussian

gamma :: Double -> Double -> Producer Double IO ()
gamma = runSampler sampleGamma

runSampler :: (GenIO -> Double -> Double -> IO Double)
           -> Double
           -> Double
           -> Producer Double IO ()
runSampler sample a b = do
  gen <- liftIO createSystemRandom
  forever $ do
    x <- liftIO $ sample gen a b
    yield x

sampleGaussian :: GenIO -> Double -> Double -> IO Double
sampleGaussian gen mu sigma = runRVar (normal mu sigma) gen

sampleGamma :: GenIO -> Double -> Double -> IO Double
sampleGamma gen alpha beta = runRVar (G.gamma alpha beta) gen
