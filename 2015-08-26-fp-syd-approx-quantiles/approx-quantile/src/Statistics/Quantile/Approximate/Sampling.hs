{-# LANGUAGE LambdaCase #-}

module Statistics.Quantile.Approximate.Sampling (
    sampling
  , samplingJackknife
) where

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.List.Ordered as Bag
import Data.Monoid
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Pipes
import qualified Pipes.Prelude as P

import qualified Statistics.Quantile as Q
import Statistics.Resampling
import Statistics.Types
import Statistics.Sample

import Statistics.Quantile.Types
import Statistics.Quantile.Util

import System.Random.MWC

data Jackknife = Jackknifed | NotJackknifed
  deriving (Eq, Show)

sampling :: Int
         -> Int
         -> Selector IO
sampling k n = Selector $ sampling' NotJackknifed k n

samplingJackknife :: Int
                  -> Int
                  -> Selector IO
samplingJackknife k n = Selector $ sampling' Jackknifed k n


-- FIXME: this breaks at 'select' with low probability.
-- FIXME: fold with something stricter than a tuple
sampling' :: Jackknife
          -> Int
          -> Int
          -> Quantile
          -> Stream IO
          -> IO Double
sampling' j k n q (Stream src) = withSystemRandom $ \gen -> do
  P.foldM (sample gen) (pure ([],0)) (finalize j) src
  where choose :: GenIO -> IO Bool
        choose g = choose' <$> uniformR (0.0, invP) g
        
        choose' :: Double -> Bool
        choose' x
          | x < 1.0   = True
          | otherwise = False

        -- 1/P(choose)
        invP :: Double
        invP = (fromIntegral n) / (fromIntegral k)

        sample :: GenIO -> ([Double], Int) -> Double -> IO ([Double], Int)
        sample g (vs,c) v = choose g >>= \case
          True -> let vs' = Bag.insertBag v vs in
                  pure (vs', c+1)
          False -> pure (vs,c)

        finalize NotJackknifed (vs, c) = pure $ vs !! (qidx q c)
        finalize Jackknifed (vs, _) = pure . estimateQ median $ jackknife (Function $ estimateQ q) $ V.fromList vs

estimateQ :: Quantile -> (Vector Double) -> Double
estimateQ (Quantile k n) = Q.weightedAvg k n
