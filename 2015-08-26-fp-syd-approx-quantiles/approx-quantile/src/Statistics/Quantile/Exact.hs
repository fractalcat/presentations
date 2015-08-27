{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Statistics.Quantile.Exact (
    external
  , external'
  , countOut
) where

import Control.Applicative
import Control.Monad.IO.Class

import Data.Monoid

import Debug.Trace

import Numeric

import Pipes
import qualified Pipes.Prelude as P

import Statistics.Quantile.Types
import Statistics.Quantile.Util

import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.FilePath
import System.Process

external :: Selector IO
external =  Selector external'

external' :: Quantile
          -> Stream IO
          -> IO Double
external' q src = do
  (fp, fh) <- openTempFile tmpDir "quantile.txt"
  n <- countOut fh src
  hClose fh
  fp' <- externalMergeSort fp
  fh' <- openFile fp' ReadMode
  quant <- select (qidx q n) $ 
                P.fromHandle fh' 
            >-> P.map realOrDie
  removeFile fp >> removeFile fp'
  pure quant
  where realOrDie :: String -> Double
        realOrDie = read

countOut :: Handle
         -> Stream IO
         -> IO Int
countOut fh (Stream src) =
  P.foldM (incrementWrite fh) (pure 0) pure src
  where incrementWrite h n v = do
          -- Can't use the Show instance because scientific notation
          -- would break sorting.
          hPutStrLn h (showFFloat Nothing v "")
          pure $! (n + 1)

select :: Monad m
       => Int
       -> Producer Double m ()
       -> m Double
select n p0 = next p0 >>= \case
  (Left _)       -> error $ show EmptySample 
  (Right (v, p)) -> go 1 v p
   where go i v p 
           | i == n    = pure v
           | otherwise = 
               next p >>= \case
                 (Left _) -> error "universe broke"
                 (Right (v', p')) -> go (i + 1) v' p'
        
externalMergeSort :: FilePath -> IO FilePath
externalMergeSort fp =
  let fp' = fp <> ".sorted" in do
  fh <- openFile fp' WriteMode
  ph <- runProcess "/usr/bin/sort" sortOpts Nothing Nothing Nothing (Just fh) Nothing
  st <- waitForProcess ph
  hClose fh
  case st of
    ExitSuccess -> pure fp'
    _ -> error $ "sort -n " <> fp <> " failed"
  where 
    -- Numeric, TMPDIR=/var/tmp so we don't fill up a tmpfs.
    sortOpts = [ "-n", "-T", "/var/tmp", fp]
