module Main where

import Options.Applicative

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC

import Statistics.Quantile.Bench.Gen (sampleGaussian)

data Params = Params Int Double Double
  deriving (Eq, Show)

params = Params <$> argument auto (metavar "COUNT")
                <*> argument auto (metavar "MEAN")
                <*> argument auto (metavar "STDDEV")

args = info (helper <*> params)
            (   fullDesc
             <> progDesc "Write COUNT real numbers drawn from a Gaussian."
            )

main :: IO ()
main = do
    ps <- execParser args
    gen <- createSystemRandom
    runEffect $ gauss gen ps >-> P.map show >-> P.stdoutLn
  where
    gauss gen (Params n mu sigma) = P.replicateM n $ sampleGaussian gen mu sigma
