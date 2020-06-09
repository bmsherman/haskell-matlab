{-|
  Array access (with EIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

|-}

module Foreign.Matlab.EIOArray where

import           ZIO.Trans

import qualified Foreign.Matlab.Array as A
import           Foreign.Matlab.Types
import           Foreign.Matlab.ZIOTypes

mxArrayClass :: MXArray a -> EIO MatlabException MXClass
mxArrayClass = mxreE . elift . A.mxArrayClass