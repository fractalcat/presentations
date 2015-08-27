module Main where

import Options.Applicative

import Pipes
import qualified Pipes.Prelude as P

import System.Random.MWC

import Statistics.Quantile.Bench.Gen (sampleGamma)

data Params = Params Int Double Double
  deriving (Eq, Show)

params = Params <$> argument auto (metavar "COUNT")
                <*> argument auto (metavar "ALPHA")
                <*> argument auto (metavar "BETA")

args = info (helper <*> params)
            (   fullDesc
             <> progDesc "Write COUNT real numbers drawn from a gamma distribution."
            )

main :: IO ()
main = do
    ps <- execParser args
    gen <- createSystemRandom
    runEffect $ gamma gen ps >-> P.map show >-> P.stdoutLn
  where
    gamma gen (Params n alpha beta) = P.replicateM n $ sampleGamma gen alpha beta
