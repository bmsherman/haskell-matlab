#
# TODO: consider using a shell-specific version of pathdef.m
# TODO: instead of relying on a default in Documents/MATLAB
#

with import <nixpkgs> {};
let
  matlabGcc = gcc49;
  matlabVersion = "R2017a";
  matlabPath = "/opt/MATLAB/${matlabVersion}";
  matlabLibPath = "${matlabPath}/bin/glnxa64";
in
stdenv.mkDerivation {
  name = "impureMatlabEnv";
  inherit matlabGcc;
  buildInputs = [
    matlabGcc
    makeWrapper
    zlib
    # for Haskell:
    cabal-install
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
  shellHook = ''
    export MATLAB_PATH=${matlabPath}
    export PATH=$PATH:$MATLAB_PATH/bin

    source ${./patchMATLAB.sh}
  '';
}
