{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, stdenv, trifecta }:
      mkDerivation {
        pname = "iso8211";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ attoparsec base bytestring trifecta pkgs.cabal-install ];
        homepage = "http://www.github.com/stepcut/iso8211";
        description = "An ISO8211 parser target towards use with IHO S-57 vector chart files";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
