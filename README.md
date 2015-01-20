This is an updated version of Dylan Simon's
[matlab](https://hackage.haskell.org/package/matlab) package which is
available on Hackage. I made changes primarily to allow the package to work
with newer versions of GHC and MATLAB.

You will probably need to some arguments that point Cabal to your MATLAB
installation. For example,
```
cabal install --extra-lib-dirs="/usr/local/MATLAB/R2014a/bin/glnxa64/" --extra-include-dirs="/usr/local/MATLAB/R2014a/extern/include/"
```

I have successfully used package with GHC 7.8.3, cabal-install 1.20.0.3, and
MATLAB R2014a on Ubuntu 14.10. To use the package on Windows, please check out
the 
[Windows branch](https://github.com/bmsherman/haskell-matlab/tree/windows32bit),
which has a hack which fixes errors which occur otherwise
(see [this issue](https://github.com/bmsherman/haskell-matlab/issues/1)).
