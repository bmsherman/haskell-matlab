This is an updated version of Dylan Simon's
[matlab](https://hackage.haskell.org/package/matlab) package which is
available on Hackage. I made changes primarily to allow the package to work
with newer versions of GHC and MATLAB.

## Installation

You will probably need to add some arguments that point Cabal to your MATLAB
installation. For example, on a Linux system with MATLAB R2014a,
it may look like this:
```
cabal install --extra-lib-dirs="/usr/local/MATLAB/R2014a/bin/glnxa64/" --extra-include-dirs="/usr/local/MATLAB/R2014a/extern/include/"
```

On a Windows system with the MATLAB Compiler Runtime, it might look like this:
```
cabal install --extra-include-dirs="C:\Program Files\MATLAB\MATLAB Compiler Runtime\v83\extern\include" --extra-lib-dirs="C:\Program Files\MATLAB\MATLAB Compiler Runtime\v83\bin\win64"
```

## Test platforms

This package has been confirmed to work on the following systems:

GHC version | cabal-install version | Operating System | MATLAB
------------|-----------------------|------------------|--------------
7.8.3       | 1.20.0.3              | Ubuntu 14.10     | MATLAB R2014a
7.8.3       | 1.18.0.5              | Windows 7        | MCR R2014a


## Stack notes

(omit the `--nix` argument if not using Nix or NixOS)

```
$ stack --extra-lib-dirs=$MATLAB_PATH/bin/glnxa64 --extra-include-dirs=$MATLAB_PATH/extern/include --nix build
```

## Nix notes

You can load a MATLAB nix shell such as the one found in `shell.nix` - feel free
to modify it to add other packages for your particular project.


Confirm it is working by running `matlab -glnxa64 -nodisplay -nosplash`.

Build the project using the build command above under "Stack notes".

## Running tests

```
LD_LIBRARY_PATH=$MATLAB_PATH/bin/glnxa64 /home/bebarker/workspace/haskell-matlab/.stack-work/install/x86_64-linux-nix/24c5769e9013838d87aa76fb4cdd10a09798b6904a6faa380de6fe6949e2c952/8.6.5/bin/matlab-engine-test
```
