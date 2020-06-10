{-|
  Array access (with EIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

|-}

module Foreign.Matlab.EIOArray (
    -- * Array manipulation
    A.anyMXArray
  , A.MNullArray, castMNull
  , mxArrayClass
  , mxArrayIsComplex
  ) where

import           Data.Complex
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import qualified Foreign.Matlab.Array as A

import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

mxArrayClass :: MXArray a -> EIO MatlabException MXClass
mxArrayClass = mxreE . elift . A.mxArrayClass

mxArrayIsComplex :: MXArray a -> EIO MatlabException Bool
mxArrayIsComplex = mxreE . elift . A.mxArrayIsComplex

castMNull :: MAnyArray -> EIO MatlabException A.MNullArray
castMNull a
  | isMNull a = pure $ unsafeCastMXArray a
  | otherwise = throwError MXNothing
