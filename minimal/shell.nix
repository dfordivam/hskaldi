{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghc;
  hsPkgs = pkgs.haskellPackages;
  stdenv = pkgs.stdenv;

  openfst = hsPkgs.callPackage ./openfst.nix {};
  kaldi = hsPkgs.callPackage ./kaldi.nix {inherit openfst;};

  drv = hsPkgs.callPackage ./default.nix {inherit kaldi openfst;};

in if pkgs.lib.inNixShell then drv.env else drv

