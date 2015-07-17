module Main where

import           Data.Monoid             ((<>))
import           Pipes
import qualified Pipes.Prelude           as P
import           System.Environment      (getArgs)
import           System.IO

import           HEP.Parser.HepMC.Parser

import           HEP.Data.HepMC.Photon   (energyDist, photonEnergies)
import           HEP.Data.Histogram1D

main :: IO ()
main = do
  args <- getArgs
  let infile = head args
  s <- withFile infile ReadMode photonSpectrum
  print s

photonSpectrum :: MonadIO m => Handle -> m (Hist1D Double)
photonSpectrum hin = P.fold (<>) mempty id spectra
  where spectra :: MonadIO m => Producer (Hist1D Double) m ()
        spectra = hepmcEvent hin >-> photonEnergies >-> energyDist 10 0 10
