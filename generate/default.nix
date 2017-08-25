{ mkDerivation, base, bytestring, fficxx, file-embed, stdenv }:
mkDerivation {
  pname = "genhskaldi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring fficxx file-embed ];
  license = stdenv.lib.licenses.bsd3;
}
