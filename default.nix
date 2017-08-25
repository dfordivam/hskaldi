{ mkDerivation, base, stdenv, bytestring, callPackage,
  fficxx,
  fficxx-runtime,
  kaldi,
  atlas,
  openfst
}:

let
  hskaldi-interface = callPackage ./hskaldi-interface.nix
    {inherit kaldi atlas openfst fficxx-runtime fficxx callPackage;};

in mkDerivation {
  pname = "hskaldi";
  version = "0.1.0.0";
  src = ./hskaldi;
  libraryHaskellDepends = [
    base bytestring fficxx fficxx-runtime hskaldi-interface
  ];
  license = stdenv.lib.licenses.bsd3;
}
