module HEP.Analysis.HepMC.Photon where

import           Control.Monad            (forever)
import           Pipes
import qualified Pipes.Prelude            as P
import           System.IO                (Handle)

import           HEP.Parser.HepMC.Parser  (hepmcEvent)
import           HEP.Parser.HepMC.Type

import           HEP.Analysis.Histogram1D

finalStates :: Monad m => Pipe GenEvent [GenParticle] m ()
finalStates = forever $ getPars >-> stable
  where getPars = P.map (map particles . vertices)
        stable = void $ await >>= yield . concatMap (filter ((==1) . statusCode))

finalPhotons :: Monad m => Pipe GenEvent [GenParticle] m ()
finalPhotons = finalStates >-> photons
  where photons = P.map (filter ((==22) . pidPDG))

photonEnergies :: Monad m => Pipe GenEvent [Double] m ()
photonEnergies = finalPhotons >-> P.map (map pE)

photonSpectrum :: MonadIO m =>
                  Int     -- ^ Number of bins
               -> Double  -- ^ Lower bound
               -> Double  -- ^ Upper bound
               -> Handle  -- ^ Handle of input file
               -> m (Hist1D Double)
photonSpectrum nbin lo hi hin = P.fold mappend mempty id spectra
  where spectra :: MonadIO m => Producer (Hist1D Double) m ()
        spectra = hepmcEvent hin >-> photonEnergies >-> energyDist nbin lo hi

energyDist :: Monad m => Int -> Double -> Double
           -> Pipe [Double] (Hist1D Double) m ()
energyDist nbin lo hi = P.map (histogram nbin lo hi)
