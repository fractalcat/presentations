module Statistics.Quantile.Util where

import Pipes
import qualified Pipes.Prelude as P

import Statistics.Quantile.Types
import System.IO

tmpDir :: FilePath
tmpDir = "/var/tmp/"

selectFromHandle :: Quantile
                 -> Selector IO
                 -> Handle
                 -> IO Double
selectFromHandle q (Selector select) hin = do
  select q $ streamHandle hin

streamHandle :: Handle -> Stream IO
streamHandle hin = Stream (P.fromHandle hin >-> P.map read)

takeStream :: Int -> Stream IO -> Stream IO
takeStream n (Stream p) = Stream $ p >-> P.take n
