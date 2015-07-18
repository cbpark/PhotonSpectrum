module Main where

import           System.Environment        (getArgs)
import           System.IO

import           HEP.Analysis.HepMC.Photon (photonSpectrum)

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  s <- withFile infile ReadMode (photonSpectrum 10 0 10)
  print s
