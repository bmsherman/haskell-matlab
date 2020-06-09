{-|
  Array access (with ZIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

|-}

module Foreign.Matlab.ZIOArray where

import           ZIO.Trans

import qualified Foreign.Matlab.EIOArray as EA
import           Foreign.Matlab.Types
import           Foreign.Matlab.ZIOTypes

mxArrayClass :: MXArray a -> ZIO r MatlabException MXClass
mxArrayClass = ezlift . EA.mxArrayClass