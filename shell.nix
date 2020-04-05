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
haskell.lib.buildStackProject {
  name = "impureMatlabEnv";
  inherit matlabGcc;
  inherit ghc;
  buildInputs = [
    matlabGcc
    makeWrapper
    zlib
    # for Haskell:
    cabal-install
    # ghc # for tests
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

# Note this will break nix commands inside the shell:
# export LD_LIBRARY_PATH=$MATLAB_PATH/bin/glnxa64:$MATLAB_PATH/sys/os/glnxa64:$LD_LIBRARY_PATH
