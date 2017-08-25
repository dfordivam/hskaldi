{ mkDerivation, base, stdenv, template-haskell, callPackage,
  fficxx,
  fficxx-runtime,
  kaldi,
  atlas,
  openfst
}:
let
  genhskaldi = callPackage ./generate {inherit fficxx;};

in mkDerivation {
  pname = "hskaldi-interface";
  version = "0.1.0.0";
  src = ./hskaldi-interface;
  libraryHaskellDepends = [
    base fficxx fficxx-runtime template-haskell
  ];
  preConfigure = ''
    ${genhskaldi}/bin/genhskaldi
    mv hskaldi-interface/* .
    '';
  librarySystemDepends = [
    kaldi atlas openfst
  ];
  license = stdenv.lib.licenses.bsd3;
}
