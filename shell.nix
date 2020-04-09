#
# TODO: consider using a shell-specific version of pathdef.m
# TODO: instead of relying on a default in Documents/MATLAB
#

with import <nixpkgs> {};
let
  deps = (import ./deps.nix);
in
haskell.lib.buildStackProject {
  name = "impureMatlabEnv";
  dontUnpack = true;
  buildInputs = deps.buildInputs;
  libPath = deps.libPath;
  src = null;
  shellHook = ''
    export MATLAB_PATH=${deps.matlabPath}
    export PATH=$PATH:$MATLAB_PATH/bin

    source ${./patchMATLAB.sh}

  '';
}

# Note this will break nix commands inside the shell:
# export LD_LIBRARY_PATH=$MATLAB_PATH/bin/glnxa64:$MATLAB_PATH/sys/os/glnxa64:$LD_LIBRARY_PATH
