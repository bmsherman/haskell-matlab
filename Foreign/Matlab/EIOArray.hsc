{-|
  Array access (with EIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

For documenation, see `Foreign.Matlab.Array`.

|-}

module Foreign.Matlab.EIOArray (
    -- * Array manipulation
    A.anyMXArray
  , A.MNullArray, castMNull
  , mxArrayClass
  , mxArrayIsComplex
  , mxArraySize
  , mxArraySetSize
  , mxArrayLength
  , freeMXArray
  , copyMXArray
  , mIndexOffset

    -- * Array element access
  , MXArrayComponent (mxArrayGetOffset, mxArraySetOffset
      , mxArrayGetOffsetList, mxArraySetOffsetList
      , mxScalarGet, isMXScalar
      , createMXArray, createMXScalar
      , createColVector, createRowVector),

  ) where

import           Data.Complex
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Matlab.Array.Internal as AI

import qualified Foreign.Matlab.Array as A

import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types
import           Foreign.Matlab.Util
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

#include <matrix.h>

mxArrayClass :: MXArray a -> EIO MatlabException MXClass
mxArrayClass = mxreE . elift . A.mxArrayClass

mxArrayIsComplex :: MXArray a -> EIO MatlabException Bool
mxArrayIsComplex = mxreE . elift . A.mxArrayIsComplex

castMNull :: MAnyArray -> EIO MatlabException A.MNullArray
castMNull a
  | isMNull a = pure $ unsafeCastMXArray a
  | otherwise = throwError MXNothing

mxArraySize :: MXArray a -> EIO MatlabException MSize
mxArraySize = mxreE . elift . A.mxArraySize

mxArraySetSize :: MXArray a -> MSize -> EIO MatlabException ()
mxArraySetSize a s = (mxreE . elift) $ A.mxArraySetSize a s

mxArrayLength :: MXArray a -> EIO MatlabException Int
mxArrayLength = mxreE . elift . A.mxArrayLength

freeMXArray :: MXArray a -> EIO MatlabException ()
freeMXArray = mxreE . elift . A.freeMXArray

copyMXArray :: MXArray a -> EIO MatlabException (MXArray a)
copyMXArray = mxreE . elift . A.copyMXArray

mIndexOffset :: MXArray a -> MIndex -> EIO MatlabException Int
mIndexOffset a i = (mxreE . elift) $ A.mIndexOffset a i


-- |The class of standardly typeable array elements
class MXArrayComponent a where
  -- |Determine whether the given array is of the correct type
  isMXArray :: MXArray a -> EIO MatlabException Bool
  -- |Create an array and initialize all its data elements to some default value, usually 0 or []
  createMXArray :: MSize -> EIO MatlabException (MXArray a)

  -- |Determine if an array is singleton. Equivalent to
  --
  -- >  liftM (all (1 ==)) . mxArraySize
  isMXScalar :: MXArray a -> EIO MatlabException Bool

  mxArrayGetOffset :: MXArray a -> Int -> EIO MatlabException a
  mxArraySetOffset :: MXArray a -> Int -> a -> EIO MatlabException ()

  mxArrayGetOffsetList :: MXArray a -> Int -> Int -> EIO MatlabException [a]
  mxArraySetOffsetList :: MXArray a -> Int -> [a] -> EIO MatlabException ()

  -- |Get the value of the first data element in an array or, more specifically, the value that the array will be interpreted as in scalar context
  mxScalarGet :: MXArray a -> EIO MatlabException a

  -- |Create a singleton (scalar) array having the specified value
  createMXScalar :: a -> EIO MatlabException (MXArray a)
  -- |Create a column vector from the given list.
  createColVector :: [a] -> EIO MatlabException (MXArray a)
  -- |Create a row vector from the given list.
  createRowVector :: [a] -> EIO MatlabException (MXArray a)

  isMXArray _ = pure False
  isMXScalar a = liftM2 (&&) (isMXArray a) (all (1 ==) =.< mxArraySize a)

  mxArrayGetOffsetList a o n = mapM (mxArrayGetOffset a) [o..o+n-1]
  mxArraySetOffsetList a o = zipWithM_ (mxArraySetOffset a . (o+)) [0..]

  mxScalarGet a = mxArrayGetOffset a 0

  createMXScalar x = do
    a <- createMXArray [1]
    mxArraySetOffset a 0 x
    pure a
  createRowVector l = do
    a <- createMXArray [1,length l]
    mxArraySetOffsetList a 0 l
    pure a
  createColVector l = do
    a <- createMXArray [length l]
    mxArraySetOffsetList a 0 l
    pure a

mxGetData :: MXArrayPtr -> UIO (Ptr a)
mxGetData = unsafeFromIO . AI.mxGetData

class (MXArrayComponent a, MType mx a, Storable mx) => MXArrayData mx a where
  withArrayData :: MXArray a -> (Ptr mx -> IO b) -> EIO MatlabException b
  withArrayData a f = mxreE . elift $ AI.withArrayDataDef a f

  withArrayDataOff :: MXArray a -> Int -> (Ptr mx -> IO b) -> EIO MatlabException b
  withArrayDataOff a o f = mxreE . elift $ AI.withArrayDataOffDef a o f

  arrayDataGet :: MXArray a -> Int -> EIO MatlabException a
  arrayDataGet a o = mxreE . elift $ AI.arrayDataGetDef a o

  arrayDataSet :: MXArray a -> Int -> a -> EIO MatlabException ()
  arrayDataSet a o v = mxreE . elift $ AI.arrayDataSetDef a o v

  arrayDataGetList :: MXArray a -> Int -> Int -> EIO MatlabException [a]
  arrayDataGetList a o n = mxreE . elift $ AI.arrayDataGetListDef a o n

  arrayDataSetList :: MXArray a -> Int -> [a] -> EIO MatlabException ()
  arrayDataSetList a o l = mxreE . elift $ AI.arrayDataSetListDef a o l

#let arrayDataComponent = "\
mxArrayGetOffset = arrayDataGet ;\
mxArraySetOffset = arrayDataSet ;\
mxArrayGetOffsetList = arrayDataGetList ;\
mxArraySetOffsetList = arrayDataSetList\
"
--"

mxIsLogical :: MXArrayPtr -> UIO CBool
mxIsLogical = unsafeFromIO . AI.mxIsLogical

mxCreateLogicalArray :: MWSize -> Ptr MWSize -> UIO MXArrayPtr
mxCreateLogicalArray sz psz = unsafeFromIO $ AI.mxCreateLogicalArray sz psz

mxGetLogicals :: MXArrayPtr -> UIO (Ptr MXLogical)
mxGetLogicals = unsafeFromIO . AI.mxGetLogicals

mxCreateLogicalScalar :: CBool -> UIO MXArrayPtr
mxCreateLogicalScalar = unsafeFromIO . AI.mxCreateLogicalScalar

mxIsLogicalScalar :: MXArrayPtr -> UIO CBool
mxIsLogicalScalar = unsafeFromIO . AI.mxIsLogicalScalar

mxIsLogicalScalarTrue :: MXArrayPtr -> UIO CBool
mxIsLogicalScalarTrue = unsafeFromIO . AI.mxIsLogicalScalarTrue

instance MXArrayComponent MLogical where
  isMXArray = mxreE . elift . AI.isMXArrayMLogical
  createMXArray = mxreE . elift . AI.createMXArrayMLogical
  createMXScalar = mxreE . elift . AI.createMXScalarMLogical
  isMXScalar = mxreE . elift . AI.isMXScalarMLogical
  mxScalarGet = mxreE . elift . AI.mxScalarGetMLogical
  #arrayDataComponent
instance MXArrayData MXLogical MLogical where
  withArrayData a f = (mxreE . elift) $ AI.withArrayDataMLogical a f

-- Checkpoint 1

instance MXArrayComponent MChar where
  isMXArray = mxreE . elift . AI.isMXArrayMChar
  createMXArray = mxreE . elift . AI.createMXArrayMChar
  createRowVector = mxreE . elift . AI.createRowVectorMChar

  #arrayDataComponent
instance MXArrayData MXChar MChar where
  withArrayData a f = (mxreE . elift) $ AI.withArrayDataMChar a f


#let numarray t = "\
instance MXArrayComponent M%s where\n\
  isMXArray a = mxreE . elift $ boolC =.< withMXArray a AI.mxIs%s\n\
  createMXArray s = mxreE . elift $ AI.withNDims s (uncurry $ AI.createNumericArray (mxClassOf (undefined :: M%s)) False) >>= mkMXArray\n\
  \
mxArrayGetOffset as i = arrayDataGet as i ;\
mxArraySetOffset as i a = arrayDataSet as i a ;\
mxArrayGetOffsetList a o n = arrayDataGetList a o n ;\
mxArraySetOffsetList a o v = arrayDataSetList a o v \
  \n\
instance MXArrayData MX%s M%s\
", #t, #t, #t, #t, #t

instance MXArrayComponent MDouble where
  isMXArray = mxreE . elift . AI.isMXArrayMDouble
  createMXScalar = mxreE . elift . AI.createMXScalarMDouble
  mxScalarGet = mxreE . elift . AI.mxScalarGetMDouble
  createMXArray = mxreE . elift . AI.createMXArrayMDouble
  #arrayDataComponent
instance MXArrayData MXDouble MDouble

#numarray Single
#numarray Int8
#numarray Int16
#numarray Int32
#numarray Int64
#numarray Uint8
#numarray Uint16
#numarray Uint32
#numarray Uint64

instance MXArrayComponent MCell where
  isMXArray = mxreE . elift . AI.isMXArrayMCell
  createMXArray = mxreE . elift . AI.createMXArrayMCell
  -- |Get a the specified cell element (not a copy).
  mxArrayGetOffset a o = mxreE . elift $ AI.mxArrayGetOffsetMCell a o
  -- |Set an element in a cell array to the specified value. The cell takes ownership of the array:
  -- |and no copy is made. Any existing value should be freed first.
  mxArraySetOffset a o mcv = mxreE . elift $ AI.mxArraySetOffsetMCell a o mcv




