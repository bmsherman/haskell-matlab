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

foreign import ccall unsafe mxIsChar :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateCharArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetChars :: MXArrayPtr -> IO (Ptr MXChar)
foreign import ccall unsafe mxCreateStringFromNChars :: CString -> MWSize -> IO MXArrayPtr

isMXArrayMChar :: MXArray MChar -> IO Bool
isMXArrayMChar a = boolC =.< withMXArray a mxIsChar

createMXArrayMChar :: MSize -> MIO (MXArray MChar)
createMXArrayMChar s = withNDims s (uncurry mxCreateCharArray) >>= mkMXArray

createRowVectorMChar :: [MChar] -> MIO (MXArray MChar)
createRowVectorMChar s = 
  mkMXArray =<< withCStringLen s (\(s,n) -> mxCreateStringFromNChars s (ii n))

withArrayDataMChar :: MXArray MChar -> (Ptr MXChar -> IO b) -> IO b
withArrayDataMChar a f = withMXArray a (mxGetChars >=> f)


foreign import ccall unsafe mxCreateNumericArray ::
  MWSize -> Ptr MWSize -> MXClassID -> (#type mxComplexity) -> IO MXArrayPtr

createNumericArray :: MXClass -> Bool -> MWSize -> Ptr MWSize -> IO MXArrayPtr
createNumericArray t c n s = mxCreateNumericArray n s (hs2mx t) (if c then (#const mxCOMPLEX) else (#const mxREAL))

foreign import ccall unsafe mxIsDouble :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateDoubleScalar :: MXDouble -> IO MXArrayPtr
foreign import ccall unsafe mxGetScalar :: MXArrayPtr -> IO MXDouble

isMXArrayMDouble :: MXArray MDouble -> IO Bool
isMXArrayMDouble a = boolC =.< withMXArray a mxIsDouble

createMXScalarMDouble :: MDouble -> MIO (MXArray MDouble)
createMXScalarMDouble = mxCreateDoubleScalar . hs2mx >=> mkMXArray

mxScalarGetMDouble :: MXArray a -> MIO MDouble
mxScalarGetMDouble a = withMXArray a mxGetScalar

createMXArrayMDouble :: MSize -> MIO (MXArray MXDouble)
createMXArrayMDouble s = withNDims s (uncurry $ createNumericArray (mxClassOf (undefined :: Double)) False) >>= mkMXArray

#let numarray t = "\
foreign import ccall unsafe mxIs%s :: MXArrayPtr -> IO CBool\n\
", #t, #t, #t, #t, #t, #t

#numarray Single
#numarray Int8
#numarray Int16
#numarray Int32
#numarray Int64
#numarray Uint8
#numarray Uint16
#numarray Uint32
#numarray Uint64
