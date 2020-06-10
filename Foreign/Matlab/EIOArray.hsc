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


-- mxGetData :: MXArrayPtr -> UEIO (Ptr a)
-- mxGetData = uelift . mxGetDataU

class (MXArrayComponent a, MType mx a, Storable mx) => MXArrayData mx a where
  withArrayData :: MXArray a -> (Ptr mx -> EIO e b) -> EIO e b
  withArrayDataOff :: MXArray a -> Int -> (Ptr mx -> EIO e b) -> EIO e b
  arrayDataGet :: MXArray a -> Int -> EIO MatlabException a
  arrayDataSet :: MXArray a -> Int -> a -> EIO MatlabException ()

  arrayDataGetList :: MXArray a -> Int -> Int -> EIO MatlabException [a]
  arrayDataSetList :: MXArray a -> Int -> [a] -> EIO MatlabException ()

  withArrayData a f = withMXArray a ((uelift . mxGetData) >=> f)
  withArrayDataOff a o f = withArrayData a (\p -> f (advancePtr p o))
  arrayDataGet a o   = withArrayDataOff a o (mx2hs .=< (mxreE . elift .peek))
  arrayDataSet a o v = withArrayDataOff a o (\p -> (mxreE . elift) $ poke p (hs2mx v))
  arrayDataGetList a o n = withArrayDataOff a o (map mx2hs .=< (mxreE . elift . (peekArray n)))
  arrayDataSetList a o l = withArrayDataOff a o (\p -> (mxreE . elift) $ pokeArray p (map hs2mx l))
#let arrayDataComponent = "\
mxArrayGetOffset = arrayDataGet ;\
mxArraySetOffset = arrayDataSet ;\
mxArrayGetOffsetList = arrayDataGetList ;\
mxArraySetOffsetList = arrayDataSetList\
"
--"
