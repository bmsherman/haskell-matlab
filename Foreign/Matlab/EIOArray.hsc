{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
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
      , createColVector, createRowVector)
    -- * Array element access
  , MXArrayComponent (mxArrayGetOffset, mxArraySetOffset
      , mxArrayGetOffsetList, mxArraySetOffsetList
      , mxScalarGet, isMXScalar
      , createMXArray, createMXScalar
      , createColVector, createRowVector)
  , castMXArray
    -- | array element access
  , mxArrayGet, mxArraySet
    -- | array list access
  , mxArrayGetList, mxArraySetList
  , mxArrayGetAll, mxArraySetAll
  , mxArrayGetOffsetSafe, mxArrayGetFirst, mxArrayGetLast
    -- mxArrayGetSafe, --TODO--
  , fromListIO, cellFromListsIO
  , isMNull

    -- * Struct access
    -- |Structs in Matlab are always arrays, and so can be accessed using most array accessors.
    -- |However, the modification functions such as 'mxArraySet' are not implemented because they could disrupt field data in the entire struct array, and so some specialized functions are necessary.
  , MStructArray
  , createStruct
  , mStructFields
  , mStructGet, mStructSet
  , mStructSetFields
  , mStructAddField, mStructRemoveField
  , mxCellGetAllOfType, mxCellGetArraysOfType

    -- ** Object access
    -- |Some structs are also validated (blessed) user objects.
  , mObjectGetClass, mObjectSetClass

  ) where

import           Data.Complex
import           Data.Either.Combinators (mapLeft)
import           Control.Exception (throw)
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


-- TODO: consider generic derivation of class instances: A.MXarrayComponent a => MXarrayComponent a

#include <matrix.h>

mxArrayClass :: MXArray a -> EIO MatlabException MXClass
mxArrayClass = mxreE . elift . A.mxArrayClass

castMNull :: MAnyArray -> EIO MatlabException A.MNullArray
castMNull a
  | isMNull a = pure $ unsafeCastMXArray a
  | otherwise = throwError MXNothing

-- |Safely cast a generic array to a type, or return Nothing if the array does not have the proper type
castMXArray :: forall a. (A.MXArrayComponent a, MXArrayComponent a) => MAnyArray -> EIO MatlabException (MXArray a)
castMXArray a = do
  aMay <- mxreE . elift $ A.castMXArray a
  case aMay of
    Just arr -> pure arr
    Nothing -> throwError MXNothing

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

-- |Get the value of the specified array element.  Does not check bounds.
mxArrayGet :: MXArrayComponent a => MXArray a -> MIndex -> EIO MatlabException a
mxArrayGet a i = mIndexOffset a i >>= mxArrayGetOffset a

-- |Set an element in an array to the specified value.  Does not check bounds.
mxArraySet :: MXArrayComponent a => MXArray a -> MIndex -> a -> EIO MatlabException ()
mxArraySet a i v = do
  o <- mIndexOffset a i
  mxArraySetOffset a o v

-- |@'mxArrayGetList' a i n@ gets the sequential list of @n@ items from array @a@ starting at index @i@.  Does not check bounds.
mxArrayGetList :: MXArrayComponent a => MXArray a -> MIndex -> Int -> EIO MatlabException [a]
mxArrayGetList a i n = do
  o <- mIndexOffset a i
  n <- if n == -1 then subtract o =.< mxArrayLength a else pure n
  mxArrayGetOffsetList a o n
-- |@'mxArraySetList' a i l@ sets the sequential items in array @a@ starting at index @i@ to @l@.  Does not check bounds.
mxArraySetList :: MXArrayComponent a => MXArray a -> MIndex -> [a] -> EIO MatlabException ()
mxArraySetList a i l = do
  o <- mIndexOffset a i
  mxArraySetOffsetList a o l

-- |Get a flat list of all elements in the array.
mxArrayGetAll :: MXArrayComponent a => MXArray a -> EIO MatlabException [a]
mxArrayGetAll a = mxArrayGetList a mStart (-1)

-- |Set a flat list of all elements in the array.
mxArraySetAll :: MXArrayComponent a => MXArray a -> [a] -> EIO MatlabException ()
mxArraySetAll a = mxArraySetList a mStart

mxArrayGetFirst :: (A.MXArrayComponent a, MXArrayComponent a) => MXArray a -> EIO MatlabException a
mxArrayGetFirst arr = mxArrayGetOffsetSafe arr 0

mxArrayGetLast :: (A.MXArrayComponent a, MXArrayComponent a) => MXArray a -> EIO MatlabException a
mxArrayGetLast arr = do
  arrLen <- mxArrayLength arr
  mxArrayGetOffsetSafe arr (arrLen - 1)

-- |Like mxArrayGetOffset but safe.
mxArrayGetOffsetSafe :: forall a. (A.MXArrayComponent a, MXArrayComponent a)
  => MXArray a -> Int -> EIO MatlabException a
mxArrayGetOffsetSafe arr ix = do
  aEi <- mxreE . elift $ A.mxArrayGetOffsetSafe arr ix
  liftEither (mapLeft MXLogicalError aEi)

-- | Create and populate an MXArray in one go. Named without 'mx' due to possible
-- | conformity to a typeclass function.
fromListIO :: (Foldable t, A.MXArrayComponent a, MXArrayComponent a)
  => t a -> EIO MatlabException (MXArray a)
fromListIO = mxreE . elift . A.fromListIO

-- | Like fromListIO but wraps elements in a cell. Most useful for converting a list of strings
-- | to a MATLAB cell array of strings. Named in conjunction with `fromListIO`, which is used
-- | as part of the implementation.
cellFromListsIO :: (Traversable s, Foldable t, A.MXArrayComponent a, MXArrayComponent a)
  => s (t a) -> EIO MatlabException (MXArray MCell)
cellFromListsIO = mxreE . elift . A.cellFromListsIO

-- | Extract all arrays of a given type from a Cell Array.
mxCellGetArraysOfType :: (A.MXArrayComponent a, MXArrayComponent a)
  => MXArray MCell -> EIO MatlabException ([MXArray a])
mxCellGetArraysOfType ca = do
  cellVals <- (fmap . fmap) mCell (mxArrayGetAll ca)
  sequence $ castMXArray <$> cellVals

-- | A convenience function to extract all arrays of a given type from a Cell Array;
-- | may have larger dimensions than the original Cell Array due to flattening.
mxCellGetAllOfType :: (A.MXArrayComponent a, MXArrayComponent a)
  => MXArray MCell -> EIO MatlabException [a]
mxCellGetAllOfType ca = do
  as <- mxCellGetArraysOfType ca
  join <$> (sequence $ mxArrayGetAll <$> as)


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

createStruct :: MSize -> [String] -> EIO MatlabException MStructArray
createStruct s f = mxreE . elift $ AI.createStruct s f

-- |Get the names of the fields
mStructFields :: MStructArray -> EIO MatlabException [String]
mStructFields = mxreE . elift . AI.mStructFields

-- |Return the contents of the named field for the given element.
-- |Returns 'MNullArray' on no such field or if the field itself is NULL
mStructGet :: MStructArray -> MIndex -> String -> EIO MatlabException MAnyArray
mStructGet a i f = mxreE . elift $ A.mStructGet a i f

-- |Sets the contents of the named field for the given element. The input is stored in the array -- no copy is made.
mStructSet :: MStructArray -> MIndex -> String -> MXArray a -> EIO MatlabException ()
mStructSet a i f v = mxreE . elift $ A.mStructSet a i f v

-- |Add a field to a structure array.
mStructAddField :: MStructArray -> String -> EIO MatlabException ()
mStructAddField a f = mxreE . elift $ A.mStructAddField a f

-- |Remove a field from a structure array. Does nothing if no such field exists.
mStructRemoveField :: MStructArray -> String -> EIO MatlabException ()
mStructRemoveField a f = mxreE . elift $ A.mStructRemoveField a f

-- |Set the fields of a struct index to the given value list.  The list corresponds to the field list and must match in size.
mStructSetFields :: MStructArray -> MIndex -> [MXArray a] -> EIO MatlabException ()
mStructSetFields a i v = mxreE . elift $ A.mStructSetFields a i v

instance MXArrayComponent MStruct where
  isMXArray = mxreE . elift . AI.isMXArrayMStruct
  createMXArray = mxreE . elift . AI.createMXArrayMStruct
  mxArrayGetOffset a o = mxreE . elift $ AI.mxArrayGetOffsetMStruct a o
  mxArraySetOffset a i s = mxreE . elift $ AI.mxArraySetOffsetMStruct a i s
  mxArrayGetOffsetList a o n = mxreE . elift $ AI.mxArrayGetOffsetListMStruct a o n
  createMXScalar = mxreE . elift . AI.createMXScalarMStruct

mObjectGetClass :: MStructArray -> EIO MatlabException (Maybe String)
mObjectGetClass = mxreE . elift . A.mObjectGetClass

-- |Set classname of an unvalidated object array.  It is illegal to call this function on a previously validated object array.
mObjectSetClass :: MStructArray -> String -> EIO MatlabException ()
mObjectSetClass a c = mxreE . elift $ A.mObjectSetClass a c

mxArrayIsComplex :: (RealFloat a, MType mx a, Storable mx, A.MXArrayComponent a)
  => MXArray (MComplex a) -> EIO MatlabException Bool
mxArrayIsComplex = mxreE . elift . A.isMXArrayMComplex

-- |Complex array access.
instance (RealFloat a, MNumeric a, MXArrayData mx a, A.MXArrayComponent a) => MXArrayComponent (MComplex a) where
  isMXArray = mxreE . elift . A.isMXArrayMComplex
  createMXArray = mxreE . elift . AI.createMXArrayMComplex
  mxArrayGetOffset a o = mxreE . elift $ AI.mxArrayGetOffsetMComplex a o
  mxArraySetOffset a o rc = mxreE . elift $ AI.mxArraySetOffsetMComplex a o rc
  mxArrayGetOffsetList a o n = mxreE . elift $ AI.mxArrayGetOffsetListMComplex a o n
  mxArraySetOffsetList a o v = mxreE . elift $ AI.mxArraySetOffsetListMComplex a o v

