{ mkDerivation, base, stdenv,
kaldi, atlas, openfst}:
mkDerivation {
  pname = "minimal";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  executableSystemDepends = [
    kaldi atlas openfst
  ];
  license = stdenv.lib.licenses.bsd3;
}
