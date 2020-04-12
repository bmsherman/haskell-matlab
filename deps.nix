with import <nixpkgs> {};
let
  matlabGcc = gcc49;
  matlabVersion = "R2017a";
  matlabPath = "/opt/MATLAB/${matlabVersion}";
  matlabLibPath = "${matlabPath}/bin/glnxa64";
in
stdenv.mkDerivation  {
  name = "impureMatlabEnvDeps";
  matlabPath = matlabPath;
  inherit matlabGcc;
  inherit ghc;
  dontUnpack = true;
  buildInputs = [
    matlabGcc
    makeWrapper
    zlib
    # for Haskell:
    gmp
    stack
  ];

  libPath = stdenv.lib.makeLibraryPath [
    gmp
    mesa_glu
    ncurses
    pam
    xorg.libxcb
    xorg.libXi
    xorg.libXext
    xorg.libXmu
    xorg.libXp
    xorg.libXpm
    xorg.libXrandr
    xorg.libXrender
    xorg.libXt
    xorg.libXtst
    xorg.libXxf86vm
    xorg.libX11
    zlib
  ];
  src = null;
}
