with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hep-utilities, HepMC, optparse-applicative
             , pipes, stdenv
             }:
             mkDerivation {
               pname = "PhotonSpectrum";
               version = "0.0.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base hep-utilities HepMC optparse-applicative pipes
               ];
               homepage = "https://github.com/cbpark/PhotonSpectrum";
               description = "Obtaining the photon energy spectrum from MC data";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
