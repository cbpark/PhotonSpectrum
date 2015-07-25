module Main where

import           Options.Applicative
import           System.IO                 (IOMode (..), withFile)

import           HEP.Analysis.HepMC.Photon (photonSpectrum)
import           HEP.Analysis.Histogram1D  (showHist1D, unitNormalize)

main :: IO ()
main = execParser opts >>= getSpectrum
  where opts = info (helper <*> cmdoptions)
               (fullDesc
                <> progDesc "Calculate photon energy spectrum from the HepMC data"
                <> header "photonspectrum - calculate photon energy spectrum")

getSpectrum :: Args -> IO ()
getSpectrum (Args infile n l u) =
  withFile infile ReadMode (photonSpectrum
                            (read n :: Int)
                            (read l :: Double)
                            (read u :: Double))
  >>= (putStr . showHist1D . unitNormalize)

data Args = Args { input :: String
                 , nbin  :: String
                 , low   :: String
                 , upper :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$>
             argument str (metavar "INPUT"
                           <> help "Input HepMC file (ex: data.hepmc)")
             <*> strOption (long "nbin"
                            <> short 'n'
                            <> metavar "nbin"
                            <> help "Number of bins")
             <*> strOption (long "low"
                            <> short 'l'
                            <> metavar "low"
                            <> help "Lower bound")
             <*> strOption (long "upper"
                            <> short 'u'
                            <> metavar "upper"
                            <> help "Upper bound")
