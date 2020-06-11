{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

module Foreign.Matlab.Array.Internal where

import           Control.Monad
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
-- import           Data.Complex
import qualified Data.Map.Strict as DM
-- import           Data.Maybe (catMaybes)
import           Foreign.Matlab.Util
import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types

#include <matrix.h>

ndims :: MWSize -> Ptr MWSize -> IO MSize
ndims n s = map ii =.< peekArray (ii n) s

withNSubs :: With MSubs (MWSize, Ptr MWSize) (IO a)
withNSubs l f = withArrayLen (map ii l) (\l a -> f (ii l, a))
withNDims :: With MSize (MWSize, Ptr MWSize) (IO a)
withNDims = withNSubs . realMSize

foreign import ccall unsafe mxGetData :: MXArrayPtr -> IO (Ptr a)

foreign import ccall unsafe mxIsLogical :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateLogicalArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetLogicals :: MXArrayPtr -> IO (Ptr MXLogical)
foreign import ccall unsafe mxCreateLogicalScalar :: CBool -> IO MXArrayPtr
foreign import ccall unsafe mxIsLogicalScalar :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxIsLogicalScalarTrue :: MXArrayPtr -> IO CBool

withArrayDataDef :: (MType mx a, Storable mx) =>
  MXArray a -> (Ptr mx -> IO b) -> IO b
withArrayDataDef a f = withMXArray a (mxGetData >=> f)

withArrayDataOffDef :: (MType mx a, Storable mx) =>
  MXArray a -> Int -> (Ptr mx -> IO b) -> IO b
withArrayDataOffDef a o f = withArrayDataDef a (\p -> f (advancePtr p o))

arrayDataGetDef :: (MType mx a, Storable mx) => MXArray a -> Int -> IO a
arrayDataGetDef a o = withArrayDataOffDef a o (mx2hs .=< peek)

arrayDataSetDef :: (MType mx a, Storable mx) => MXArray a -> Int -> a -> IO ()
arrayDataSetDef a o v = withArrayDataOffDef a o (\p -> poke p (hs2mx v))

arrayDataGetListDef :: (MType mx a, Storable mx) => MXArray a -> Int -> Int -> IO [a]
arrayDataGetListDef a o n = withArrayDataOffDef a o (map mx2hs .=< peekArray n)

arrayDataSetListDef :: (MType mx a, Storable mx) => MXArray a -> Int -> [a] -> IO ()
arrayDataSetListDef a o l = withArrayDataOffDef a o (\p -> pokeArray p (map hs2mx l))

isMXArrayMLogical :: MXArray MLogical -> IO Bool
isMXArrayMLogical a = boolC =.< withMXArray a mxIsLogical

createMXArrayMLogical :: MSize -> MIO (MXArray MLogical)
createMXArrayMLogical s = withNDims s (uncurry mxCreateLogicalArray) >>= mkMXArray

createMXScalarMLogical :: MLogical -> MIO (MXArray MLogical)
createMXScalarMLogical = mxCreateLogicalScalar . cBool >=> mkMXArray

isMXScalarMLogical :: MXArray a -> MIO Bool
isMXScalarMLogical a = boolC =.< withMXArray a mxIsLogicalScalar

mxScalarGetMLogical :: MXArray a -> MIO MLogical
mxScalarGetMLogical a = boolC =.< withMXArray a mxIsLogicalScalarTrue

withArrayDataMLogical :: MXArray MLogical -> (Ptr MXLogical -> IO b) -> IO b
withArrayDataMLogical a f = withMXArray a (mxGetLogicals >=> f)
