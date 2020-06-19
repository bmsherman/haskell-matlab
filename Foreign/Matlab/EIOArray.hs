{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MonoLocalBinds #-}
{-|
  Array access (with EIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

For underlying implementation, see `Foreign.Matlab.Array`.

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
  , A.MXArrayComponent
  , isMXArray, mxArrayGetOffset, mxArraySetOffset
    , mxArrayGetOffsetList, mxArraySetOffsetList
    , mxScalarGet, isMXScalar
    , createMXArray, createMXScalar
    , createColVector, createRowVector  
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

import           Data.Either.Combinators (mapLeft)
import qualified Foreign.Matlab.Array.Internal as AI

import qualified Foreign.Matlab.Array as A

import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types
import           Foreign.Matlab.Util
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

-- |Return the representation of the type of the elements of an array
mxArrayClass :: MXArray a -> EIO MatlabException MXClass
mxArrayClass = mxreE . elift . A.mxArrayClass

-- |Safely cast a generic array to a NULL array, or return Nothing if the array is not NULL
castMNull :: MAnyArray -> EIO MatlabException A.MNullArray
castMNull a
  | isMNull a = pure $ unsafeCastMXArray a
  | otherwise = throwError MXNothing

-- |Safely cast a generic array to a type, or return Nothing if the array does not have the proper type
castMXArray :: forall a. A.MXArrayComponent a => MAnyArray -> EIO MatlabException (MXArray a)
castMXArray a = do
  aMay <- mxreE . elift $ A.castMXArray a
  case aMay of
    Just arr -> pure arr
    Nothing -> throwError MXNothing

-- |Get the size (dimensions) of an array
mxArraySize :: MXArray a -> EIO MatlabException MSize
mxArraySize = mxreE . elift . A.mxArraySize

-- |Set dimension array and number of dimensions
mxArraySetSize :: MXArray a -> MSize -> EIO MatlabException ()
mxArraySetSize a s = (mxreE . elift) $ A.mxArraySetSize a s

-- |Like `numel` in MATLAB.
mxArrayLength :: MXArray a -> EIO MatlabException Int
mxArrayLength = mxreE . elift . A.mxArrayLength

-- |Destroy an array and all of its contents.
freeMXArray :: MXArray a -> EIO MatlabException ()
freeMXArray = mxreE . elift . A.freeMXArray

-- |Make a deep copy of an array
copyMXArray :: MXArray a -> EIO MatlabException (MXArray a)
copyMXArray = mxreE . elift . A.copyMXArray

-- |Get the value of the specified array element.  Does not check bounds.
mIndexOffset :: MXArray a -> MIndex -> EIO MatlabException Int
mIndexOffset a i = (mxreE . elift) $ A.mIndexOffset a i

-- |Get the value of the specified array element.  Does not check bounds.
mxArrayGet :: A.MXArrayComponent a => MXArray a -> MIndex -> EIO MatlabException a
mxArrayGet a i = mIndexOffset a i >>= mxArrayGetOffset a

-- |Set an element in an array to the specified value.  Does not check bounds.
mxArraySet :: A.MXArrayComponent a => MXArray a -> MIndex -> a -> EIO MatlabException ()
mxArraySet a i v = do
  o <- mIndexOffset a i
  mxArraySetOffset a o v

-- |@'mxArrayGetList' a i n@ gets the sequential list of @n@ items from array @a@ starting at index @i@.  Does not check bounds.
mxArrayGetList :: A.MXArrayComponent a => MXArray a -> MIndex -> Int -> EIO MatlabException [a]
mxArrayGetList a i n = do
  o <- mIndexOffset a i
  n <- if n == -1 then subtract o =.< mxArrayLength a else pure n
  mxArrayGetOffsetList a o n
-- |@'mxArraySetList' a i l@ sets the sequential items in array @a@ starting at index @i@ to @l@.  Does not check bounds.
mxArraySetList :: A.MXArrayComponent a => MXArray a -> MIndex -> [a] -> EIO MatlabException ()
mxArraySetList a i l = do
  o <- mIndexOffset a i
  mxArraySetOffsetList a o l

-- |Get a flat list of all elements in the array.
mxArrayGetAll :: A.MXArrayComponent a => MXArray a -> EIO MatlabException [a]
mxArrayGetAll a = mxArrayGetList a mStart (-1)

-- |Set a flat list of all elements in the array.
mxArraySetAll :: A.MXArrayComponent a => MXArray a -> [a] -> EIO MatlabException ()
mxArraySetAll a = mxArraySetList a mStart

mxArrayGetFirst :: A.MXArrayComponent a => MXArray a -> EIO MatlabException a
mxArrayGetFirst arr = mxArrayGetOffsetSafe arr 0

mxArrayGetLast :: A.MXArrayComponent a => MXArray a -> EIO MatlabException a
mxArrayGetLast arr = do
  arrLen <- mxArrayLength arr
  mxArrayGetOffsetSafe arr (arrLen - 1)

-- |Like mxArrayGetOffset but safe.
mxArrayGetOffsetSafe :: forall a. A.MXArrayComponent a
  => MXArray a -> Int -> EIO MatlabException a
mxArrayGetOffsetSafe arr ix = do
  aEi <- mxreE . elift $ A.mxArrayGetOffsetSafe arr ix
  liftEither (mapLeft MXLogicalError aEi)

-- | Create and populate an MXArray in one go. Named without 'mx' due to possible
-- | conformity to a typeclass function.
fromListIO :: (Foldable t, A.MXArrayComponent a)
  => t a -> EIO MatlabException (MXArray a)
fromListIO = mxreE . elift . A.fromListIO

-- | Like fromListIO but wraps elements in a cell. Most useful for converting a list of strings
-- | to a MATLAB cell array of strings. Named in conjunction with `fromListIO`, which is used
-- | as part of the implementation.
cellFromListsIO :: (Traversable s, Foldable t, A.MXArrayComponent a)
  => s (t a) -> EIO MatlabException (MXArray MCell)
cellFromListsIO = mxreE . elift . A.cellFromListsIO

-- | Extract all arrays of a given type from a Cell Array.
mxCellGetArraysOfType :: (A.MXArrayComponent a)
  => MXArray MCell -> EIO MatlabException ([MXArray a])
mxCellGetArraysOfType ca = do
  cellVals <- (fmap . fmap) mCell (mxArrayGetAll ca)
  sequence $ castMXArray <$> cellVals

-- | A convenience function to extract all arrays of a given type from a Cell Array;
-- | may have larger dimensions than the original Cell Array due to flattening.
mxCellGetAllOfType :: (A.MXArrayComponent a)
  => MXArray MCell -> EIO MatlabException [a]
mxCellGetAllOfType ca = do
  as <- mxCellGetArraysOfType ca
  join <$> (sequence $ mxArrayGetAll <$> as)


-- |The class of standardly typeable array elements

-- |Determine whether the given array is of the correct type
isMXArray :: A.MXArrayComponent a => MXArray a -> EIO MatlabException Bool
-- |Create an array and initialize all its data elements to some default value, usually 0 or []
createMXArray :: A.MXArrayComponent a => MSize -> EIO MatlabException (MXArray a)

-- |Determine if an array is singleton. Equivalent to
--
-- >  liftM (all (1 ==)) . mxArraySize
isMXScalar :: A.MXArrayComponent a => MXArray a -> EIO MatlabException Bool

mxArrayGetOffset :: A.MXArrayComponent a => MXArray a -> Int -> EIO MatlabException a
mxArraySetOffset :: A.MXArrayComponent a => MXArray a -> Int -> a -> EIO MatlabException ()

mxArrayGetOffsetList :: A.MXArrayComponent a => MXArray a -> Int -> Int -> EIO MatlabException [a]
mxArraySetOffsetList :: A.MXArrayComponent a => MXArray a -> Int -> [a] -> EIO MatlabException ()

-- |Get the value of the first data element in an array or, more specifically, the value that the array will be interpreted as in scalar context
mxScalarGet :: A.MXArrayComponent a => MXArray a -> EIO MatlabException a

-- |Create a singleton (scalar) array having the specified value
createMXScalar :: A.MXArrayComponent a => a -> EIO MatlabException (MXArray a)
-- |Create a column vector from the given list.
createColVector :: A.MXArrayComponent a => [a] -> EIO MatlabException (MXArray a)
-- |Create a row vector from the given list.
createRowVector :: A.MXArrayComponent a => [a] -> EIO MatlabException (MXArray a)

createColVector l = do
  a <- createMXArray [length l]
  mxArraySetOffsetList a 0 l
  pure a

-- mxGetData :: MXArrayPtr -> UIO (Ptr a)
-- mxGetData = unsafeFromIO . AI.mxGetData

-- class (MXArrayComponent a, MType mx a, Storable mx) => MXArrayData mx a where
--   withArrayData :: MXArray a -> (Ptr mx -> IO b) -> EIO MatlabException b
--   withArrayData a f = mxreE . elift $ AI.withArrayDataDef a f

--   withArrayDataOff :: MXArray a -> Int -> (Ptr mx -> IO b) -> EIO MatlabException b
--   withArrayDataOff a o f = mxreE . elift $ AI.withArrayDataOffDef a o f

--   arrayDataGet :: MXArray a -> Int -> EIO MatlabException a
--   arrayDataGet a o = mxreE . elift $ AI.arrayDataGetDef a o

--   arrayDataSet :: MXArray a -> Int -> a -> EIO MatlabException ()
--   arrayDataSet a o v = mxreE . elift $ AI.arrayDataSetDef a o v

--   arrayDataGetList :: MXArray a -> Int -> Int -> EIO MatlabException [a]
--   arrayDataGetList a o n = mxreE . elift $ AI.arrayDataGetListDef a o n

--   arrayDataSetList :: MXArray a -> Int -> [a] -> EIO MatlabException ()
--   arrayDataSetList a o l = mxreE . elift $ AI.arrayDataSetListDef a o l

-- instance A.MXArrayComponent a => MXArrayComponent a where
isMXArray = mxreE . elift . A.isMXArray
createMXArray = mxreE . elift . A.createMXArray
createMXScalar = mxreE . elift . A.createMXScalar
isMXScalar = mxreE . elift . A.isMXScalar
mxScalarGet = mxreE . elift . A.mxScalarGet
createRowVector = mxreE . elift . A.createRowVector
mxArrayGetOffset a o = mxreE . elift $ A.mxArrayGetOffset a o
mxArraySetOffset a o mcv = mxreE . elift $ A.mxArraySetOffset a o mcv
mxArrayGetOffsetList a o n = mxreE . elift $ A.mxArrayGetOffsetList a o n
mxArraySetOffsetList a o v = mxreE . elift $ A.mxArraySetOffsetList a o v

-- instance MXArrayData MXLogical MLogical where
--   withArrayData a f = (mxreE . elift) $ AI.withArrayDataMLogical a f

-- Checkpoint 1

-- instance MXArrayData MXChar MChar where
--   withArrayData a f = (mxreE . elift) $ AI.withArrayDataMChar a f


-- instance MXArrayData MXDouble MDouble

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

mObjectGetClass :: MStructArray -> EIO MatlabException (Maybe String)
mObjectGetClass = mxreE . elift . A.mObjectGetClass

-- |Set classname of an unvalidated object array.  It is illegal to call this function on a previously validated object array.
mObjectSetClass :: MStructArray -> String -> EIO MatlabException ()
mObjectSetClass a c = mxreE . elift $ A.mObjectSetClass a c

mxArrayIsComplex :: A.MXArrayComponent (MComplex a)
  => MXArray (MComplex a) -> EIO MatlabException Bool
mxArrayIsComplex = mxreE . elift . A.isMXArray

