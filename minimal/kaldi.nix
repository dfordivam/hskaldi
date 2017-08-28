{
  stdenv,
  binutils,
  which,
  fetchgit,
  python27,
  atlas,
  speex,
  cudatoolkit,
  openfst
}:

stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  pname = "kaldi";
  version = "5.2.5";
  meta = with stdenv.lib; {
    inherit version;
    description = "Toolkit for speech recognition";
    homepage = http://kaldi-asr.org/;
    license = licenses.asl20;
    maintainers = with stdenv.lib.maintainers; [ dfordivam ];
    platforms = platforms.x86_64;
  };

  src = fetchgit {
    url = "https://github.com/kaldi-asr/kaldi.git";
    rev = "3e57783cf850fbe05e8bc45a563f12fca8a1cead";
    sha256 = "1sq9z60qiidkrpxgy5nk6x86wh9ljsm3yi1gf4nnmrwvaww3by9l";
  };

  buildInputs = [ atlas cudatoolkit openfst python27 ];
  nativeBuildInputs = [ which binutils ];

  enableParallelBuilding = true;

  postUnpack = "sourceRoot=\${sourceRoot}/src";

  patchPhase = ''
    sed 's=/bin/bash=/${stdenv.shell}=g' -i `find -type f`
  '';

  CXX = "g++";
  dontAddPrefix = true;

  preConfigure = ''
    export CXX=${stdenv.cc.cc}/bin/g++;
  '';

  LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [stdenv.cc.libc];
  LIBRARY_PATH = stdenv.lib.makeLibraryPath [stdenv.cc.libc];

  configureFlags = [
    "--fst-root=${openfst}"
    # the configure script tries to search for Makefile if the version is not specified
    "--fst-version=1.6.3"
    "--speex-root=${speex.dev}"
    "--atlas-root=${atlas}"
    "--cudatk-dir=${cudatoolkit}"
    "--shared"
  ];

  preBuild = ''
    make depend
  '';


  installPhase = ''
    mkdir -p $out/{include,lib};
    rm -rf ./lib;
    find . -name \*.h -exec cp --parents {} $out/include \;
    find . -name \*.so -exec cp {} $out/lib \;
  '';
}