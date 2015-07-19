module Main where

import           System.Environment        (getArgs)
import           System.IO

import           HEP.Analysis.HepMC.Photon (photonSpectrum)
import           HEP.Analysis.Histogram1D  (showHist1D)

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  s <- withFile infile ReadMode (photonSpectrum 10 0 10)
  putStr $ showHist1D s
