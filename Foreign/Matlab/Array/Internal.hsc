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

foreign import ccall unsafe mxIsCell :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateCellArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetCell :: MXArrayPtr -> MWIndex -> IO MXArrayPtr
foreign import ccall unsafe mxSetCell :: MXArrayPtr -> MWIndex -> MXArrayPtr -> IO ()

isMXArrayMCell :: MXArray MCell -> IO Bool
isMXArrayMCell a = boolC =.< withMXArray a mxIsCell

createMXArrayMCell :: MSize -> MIO (MXArray MCell)
createMXArrayMCell s = withNDims s (uncurry mxCreateCellArray) >>= mkMXArray

-- |Get a the specified cell element (not a copy).
mxArrayGetOffsetMCell :: MXArray MCell -> Int -> MIO MCell
mxArrayGetOffsetMCell a o = withMXArray a (\a -> mxGetCell a (ii o) >>= mkMXArray >.= MCell)

-- |Set an element in a cell array to the specified value. The cell takes ownership of the array: and no copy is made. Any existing value should be freed first.
mxArraySetOffsetMCell :: MXArray MCell -> Int -> MCell -> MIO ()
mxArraySetOffsetMCell a o (MCell v) = withMXArray a (\a -> withMXArray v (mxSetCell a (ii o)))

foreign import ccall unsafe mxIsStruct :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxIsObject :: MXArrayPtr -> IO CBool

foreign import ccall unsafe mxCreateStructArray :: MWSize -> Ptr MWSize -> CInt -> Ptr CString -> IO MXArrayPtr

foreign import ccall unsafe mxGetNumberOfFields :: MXArrayPtr -> IO CInt
foreign import ccall unsafe mxGetFieldNameByNumber :: MXArrayPtr -> CInt -> IO CString
foreign import ccall unsafe mxGetFieldNumber :: MXArrayPtr -> CString -> IO CInt

foreign import ccall unsafe mxGetField :: MXArrayPtr -> MWIndex -> CString -> IO MXArrayPtr
foreign import ccall unsafe mxSetField :: MXArrayPtr -> MWIndex -> CString -> MXArrayPtr -> IO ()
foreign import ccall unsafe mxGetFieldByNumber :: MXArrayPtr -> MWIndex -> CInt -> IO MXArrayPtr
foreign import ccall unsafe mxSetFieldByNumber :: MXArrayPtr -> MWIndex -> CInt -> MXArrayPtr -> IO ()

foreign import ccall unsafe mxAddField :: MXArrayPtr -> CString -> IO CInt
foreign import ccall unsafe mxRemoveField :: MXArrayPtr -> CInt -> IO ()

-- |Create an N-Dimensional structure array having the specified fields; initialize all values to 'MNullArray'
createStruct :: MSize -> [String] -> MIO MStructArray
createStruct s f =
  withNDims s (\(nd,d) ->
    mapWithArrayLen withCString f (\(f,nf) ->
      mxCreateStructArray nd d (ii nf) f))
  >>= mkMXArray

-- |Get the names of the fields
mStructFields :: MStructArray -> MIO [String]
mStructFields a = withMXArray a $ \a -> do
  n <- mxGetNumberOfFields a
  forM [0..pred n] (mxGetFieldNameByNumber a >=> peekCString)

structGetOffsetFields :: MStructArray -> [String] -> Int -> IO MStruct
structGetOffsetFields a f o =
  MStruct =.< withMXArray a (\a -> DM.fromList <$>
    (zipWithM (\f -> ((,) f) .=< (mxGetFieldByNumber a (ii o) >=> mkMXArray)) f [0..]))

isMXArrayMStruct :: MXArray MStruct -> IO Bool
isMXArrayMStruct a = liftM2 (||) (boolC =.< withMXArray a mxIsStruct) (boolC =.< withMXArray a mxIsObject)

createMXArrayMStruct :: MSize -> MIO (MXArray MStruct)
createMXArrayMStruct s = createStruct s []

mxArrayGetOffsetMStruct :: MXArray MStruct -> Int -> MIO MStruct
mxArrayGetOffsetMStruct a o = do
  f <- mStructFields a
  structGetOffsetFields a f o

mxArraySetOffsetMStruct :: MXArray MStruct -> Int -> MStruct -> MIO ()
mxArraySetOffsetMStruct = error "mxArraySet undefined for MStruct: use mStructSet" -- arrayDataSetDef


mxArrayGetOffsetListMStruct :: MXArray MStruct -> Int -> Int -> MIO [MStruct]
mxArrayGetOffsetListMStruct a o n = do
  f <- mStructFields a
  mapM (structGetOffsetFields a f) [o..o+n-1]

createMXScalarMStruct :: MStruct -> MIO (MXArray MStruct)
createMXScalarMStruct (MStruct fv) = do
  a <- createStruct [1] f
  withMXArray a $ \a -> zipWithM_ (\i v -> withMXArray v (mxSetFieldByNumber a 0 i)) [0..] v
  pure a
  where
    (f,v) = unzip $ DM.toList fv
