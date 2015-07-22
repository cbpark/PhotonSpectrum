with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, HepMC, optparse-applicative, pipes, stdenv
             , vector
             }:
             mkDerivation {
               pname = "PhotonSpectrum";
               version = "0.0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base HepMC optparse-applicative pipes vector ];
               homepage = "https://github.com/cbpark/PhotonSpectrum";
               description = "Obtaining the photon energy spectrum from MC data";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
