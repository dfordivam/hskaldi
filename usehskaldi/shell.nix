{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghc;
  hsPkgs = pkgs.haskellPackages;
  stdenv = pkgs.stdenv;

  fficxx = hsPkgs.callPackage ../../fficxx/fficxx/default.nix {};
  fficxx-runtime = hsPkgs.callPackage ../../fficxx/fficxx-runtime/default.nix {};
  openfst = hsPkgs.callPackage ./openfst.nix {};
  kaldi = hsPkgs.callPackage ./kaldi.nix {inherit openfst;};

  hskaldi = hsPkgs.callPackage ../default.nix {inherit fficxx openfst kaldi; callPackage = hsPkgs.callPackage;};

  drv = hsPkgs.callPackage (hsPkgs.haskellSrc2nix {
  name = "usehskaldi";
  src = ./.;}) {
    inherit hskaldi;
    };
in if pkgs.lib.inNixShell then drv.env else drv

