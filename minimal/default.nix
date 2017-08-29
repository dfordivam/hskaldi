{ mkDerivation, base, stdenv,
kaldi, atlas, openfst, HCodecs, bytestring, array}:
mkDerivation {
  pname = "minimal";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring array HCodecs ];
  executableSystemDepends = [
    kaldi atlas openfst
  ];
  license = stdenv.lib.licenses.bsd3;
}
