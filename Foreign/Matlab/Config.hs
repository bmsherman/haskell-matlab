module Foreign.Matlab.Config (matlabPath, matlabBin, matlabArch, dllExtension) where

import Distribution.Simple.BuildPaths (dllExtension)

matlabPath :: FilePath
matlabBin :: FilePath
matlabArch :: String

matlabPath = "/usr/local/MATLAB/R2013a"
matlabBin = "/usr/local/MATLAB/R2013a/bin/matlab"
matlabArch = "glnxa64"
