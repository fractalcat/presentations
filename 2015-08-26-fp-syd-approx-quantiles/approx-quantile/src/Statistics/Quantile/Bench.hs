module Statistics.Quantile.Bench where

import Control.Applicative

import Criterion

import Pipes
import qualified Pipes.Prelude as P

import Statistics.Quantile.Types
import Statistics.Quantile.Util

import System.IO

dummy :: Selector IO
dummy = Selector dummy'

dummy' :: Quantile -> Stream IO -> IO Double
dummy' _ (Stream src) = do
  runEffect $ src >-> P.map show >-> P.stdoutLn
  pure 0

benchSelector :: [Int]
              -> String
              -> Quantile
              -> Selector IO
              -> FilePath
              -> Benchmark
benchSelector ns name q (Selector select) fp = 
  bgroup name $ benchSelector' <$> ns
  where benchSelector' n = bench' n fp (select q)

benchBufferingSelector :: (Int -> Int)
                       -> [Int]
                       -> String
                       -> Quantile
                       -> (Int -> Int -> Selector IO)
                       -> FilePath
                       -> Benchmark
benchBufferingSelector f ns name q select fp = 
  bgroup name $ benchSelector' <$> ns
  where benchSelector' n = bench' n fp (select' n q)

        select' n = unSelector  (select (f n) n)

bench' :: Int -> FilePath -> (Stream IO -> IO Double) -> Benchmark
bench' n fp f = do
  bench (show n) $ nfIO $ do
    fh <- liftIO $ openFile fp ReadMode
    (f . takeStream n $ streamHandle fh)

bufSize :: (Int -> Int)
        -> Int
        -> Int
bufSize f n
  | n < 10000 = n
  | otherwise = f n

sqrtN :: Int -> Int
sqrtN = bufSize (floor . sqrt . fromIntegral)

logN :: Int -> Int
logN = bufSize (floor . logBase 2 . fromIntegral)

