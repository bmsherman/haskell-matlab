This is an updated version of Dylan Simon's
[matlab](https://hackage.haskell.org/package/matlab) package which is
available on Hackage. I made changes primarily to allow the package to work
with newer versions of GHC and MATLAB.

## Installation

### Cabal
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

### Stack

These instructiosn are much the same as the Cabal instructions above.

(omit the `--nix` argument if not using Nix or NixOS)

```
$ stack --extra-lib-dirs=$MATLAB_PATH/bin/glnxa64 --extra-include-dirs=$MATLAB_PATH/extern/include --nix build
```

### Nix

**IMPORTANT Caveat:** If it is necessary to specify the `-glnxa64` option,
the `matlab` shell script should instead be modified to hardcode `ARCH` as
`glnxa64`. Aside from convenience, certain APIs like the MATLAB Engine API
will not always work otherwise (you will likely see an error involving `trap`).

These instructions assume `stack` is to be used in a Nix environment. If you don't already
have stack in our environment you can add it, or likely you can just run `nix-shell deps.nix`
to load stack and other dependencies.

You can load a MATLAB nix shell such as the one found in `shell.nix` - feel free
to modify it to add other packages for your particular project; in this case `shell.nix`
depends on `deps.nix`. What is important to keep in mind about `shell.nix` is that it is
configured to be used by `stack` in the `stack.yaml` file, but you could also run
`nix-shell shell.nix` (or simply `nix-shell` in the current directory) to load an environment
in which you could run MATLAB as well.

Confirm it is working by running `matlab -nodisplay -nosplash`.

Build the project using the build command above under "Stack notes".

## Test platforms

This package has been confirmed to work on the following systems:

GHC version | cabal-install (c) or stack (s) version | Operating System | MATLAB
------------|----------------------------------------|------------------|--------------
8.6.5       | 2.1.3.1 (s)                            | Ubuntu 18.04     | MATLAB R2018a
8.6.5       | 2.1.3.1 (s)                            | NixOS 19.09      | MATLAB R2017a
7.8.3       | 1.20.0.3 (c)                           | Ubuntu 14.10     | MATLAB R2014a
7.8.3       | 1.18.0.5 (c)                           | Windows 7        | MCR R2014a

## Running tests

### Engine test

Note that this requires shell program `csh` to be installed at `/bin/csh`.
This is a requirement of the MATLAB Engine API. Instead of `csh`, `tcsh`
may also be used as long as `/bin/csh` points to the `tcsh` executable.


### All tests

#### Option 1

After installing, you can run a particular exectuable (e.g. `matlab-engine-test`) using the `runHSMat.sh`
script. You need to make sure `MATLAB_PATH` is set correctly or passed in.

```
stack --nix install
./runHSMat.sh matlab-engine-test

```


#### Option 2  (not working yet)


```
LD_LIBRARY_PATH=$MATLAB_PATH/bin/glnxa64 /home/bebarker/workspace/haskell-matlab/.stack-work/install/x86_64-linux-nix/24c5769e9013838d87aa76fb4cdd10a09798b6904a6faa380de6fe6949e2c952/8.6.5/bin/matlab-engine-test
```


# MATLAB Documentation

## Engine API

See [MATLAB Engine API for C](https://www.mathworks.com/help/matlab/calling-matlab-engine-from-c-programs-1.html)
for details.
