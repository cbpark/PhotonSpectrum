with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, HepMC, pipes, stdenv }:
             mkDerivation {
               pname = "PhotonSpectrum";
               version = "0.0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base HepMC pipes ];
               homepage = "https://github.com/cbpark/PhotonSpectrum";
               description = "Obtaining the photon energy spectrum from MC data";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env