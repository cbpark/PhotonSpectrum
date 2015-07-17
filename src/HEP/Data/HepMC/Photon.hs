module HEP.Data.HepMC.Photon where

import           Control.Monad         (forever)
import           Pipes
import qualified Pipes.Prelude         as P

import           HEP.Parser.HepMC.Type

import           HEP.Data.Histogram1D

finalStates :: Monad m => Pipe GenEvent [GenParticle] m ()
finalStates = forever $ getPars >-> stable
  where getPars = P.map (map particles . vertices)
        stable = void $ await >>= yield . concatMap (filter ((==1) . statusCode))

finalPhotons :: Monad m => Pipe GenEvent [GenParticle] m ()
finalPhotons = finalStates >-> photons
  where photons = P.map (filter ((==22) . pidPDG))

photonEnergies :: Monad m => Pipe GenEvent [Double] m ()
photonEnergies = finalPhotons >-> P.map (map pE)

energyDist :: Monad m =>
              Int     -- ^ Number of bins
           -> Double  -- ^ Lower bound
           -> Double  -- ^ Upper bound
           -> Pipe [Double] (Hist1D Double) m ()
energyDist nbin lo hi = P.map (histogram nbin lo hi)
