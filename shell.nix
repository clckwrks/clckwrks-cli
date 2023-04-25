with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            ({ mkDerivation, acid-state, base, clckwrks, haskeline, mtl
             , network, parsec, stdenv, cabal-install
             }:
             mkDerivation {
               pname = "clckwrks-cli";
               version = "0.2.14";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 acid-state base  haskeline mtl network parsec cabal-install  clckwrks
               ];
               homepage = "http://www.clckwrks.com/";
               description = "a command-line interface for adminstrating some aspects of clckwrks";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
