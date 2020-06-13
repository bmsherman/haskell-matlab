{-# LANGUAGE FlexibleContexts, MonoLocalBinds, ScopedTypeVariables, UndecidableInstances #-}
{-|
  Array access (with ZIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

For documenation, see `Foreign.Matlab.Array`.

|-}

module Foreign.Matlab.ZIOArray where

import           Data.Complex
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Matlab.Array.Internal as AI

import qualified Foreign.Matlab.Array as A
import qualified Foreign.Matlab.EIOArray as EA

import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types
import           Foreign.Matlab.Util
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

mxArrayClass :: MXArray a -> ZIO r MatlabException MXClass
mxArrayClass = ezlift . EA.mxArrayClass

mxArrayIsComplex :: A.MXArrayComponent (MComplex a)
  => MXArray (MComplex a) -> ZIO r MatlabException Bool
mxArrayIsComplex = ezlift . EA.mxArrayIsComplex
