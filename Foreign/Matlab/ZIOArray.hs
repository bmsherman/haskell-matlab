{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MonoLocalBinds #-}
{-|
  Array access (with ZIO wrappers), including cell arrays and structures.

Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.

For underlying implementation, see `Foreign.Matlab.Array`.

|-}
module Foreign.Matlab.ZIOArray (
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
  , mxCellGetAllOfType, mxCellGetAllListsOfType, mxCellGetArraysOfType

    -- ** Object access
    -- |Some structs are also validated (blessed) user objects.
  , mObjectGetClass, mObjectSetClass
  ) where

import qualified Foreign.Matlab.Array as A
import qualified Foreign.Matlab.EIOArray as EA hiding (MXArrayComponent)

import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types
import           Foreign.Matlab.Util
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

-- |Return the representation of the type of the elements of an array
mxArrayClass :: MXArray a -> ZIO r MatlabException MXClass
mxArrayClass = ezlift . EA.mxArrayClass

-- |Safely cast a generic array to a NULL array, or return Nothing if the array is not NULL
castMNull :: MAnyArray -> ZIO r MatlabException A.MNullArray
castMNull a
  | isMNull a = pure $ unsafeCastMXArray a
  | otherwise = throwError $ MXNothing "castMNull"

-- |Safely cast a generic array to a type, or return Nothing if the array does not have the proper type
castMXArray :: forall r a. A.MXArrayComponent a => MAnyArray -> ZIO r MatlabException (MXArray a)
castMXArray = ezlift . EA.castMXArray

-- |Get the size (dimensions) of an array
mxArraySize :: MXArray a -> ZIO r MatlabException MSize
mxArraySize = ezlift . EA.mxArraySize

-- |Set dimension array and number of dimensions
mxArraySetSize :: MXArray a -> MSize -> ZIO r MatlabException ()
mxArraySetSize a s = ezlift $ EA.mxArraySetSize a s

-- |Like `numel` in MATLAB.
mxArrayLength :: MXArray a -> ZIO r MatlabException Int
mxArrayLength = ezlift . EA.mxArrayLength

-- |Destroy an array and all of its contents.
freeMXArray :: MXArray a -> ZIO r MatlabException ()
freeMXArray = ezlift . EA.freeMXArray

-- |Make a deep copy of an array
copyMXArray :: MXArray a -> ZIO r MatlabException (MXArray a)
copyMXArray = ezlift . EA.copyMXArray

-- |Get the value of the specified array element.  Does not check bounds.
mIndexOffset :: MXArray a -> MIndex -> ZIO r MatlabException Int
mIndexOffset a i = ezlift $ EA.mIndexOffset a i

-- |Get the value of the specified array element.  Does not check bounds.
mxArrayGet :: A.MXArrayComponent a => MXArray a -> MIndex -> ZIO r MatlabException a
mxArrayGet a i = mIndexOffset a i >>= mxArrayGetOffset a

-- |Set an element in an array to the specified value.  Does not check bounds.
mxArraySet :: A.MXArrayComponent a => MXArray a -> MIndex -> a -> ZIO r MatlabException ()
mxArraySet a i v = do
  o <- mIndexOffset a i
  mxArraySetOffset a o v

-- |@'mxArrayGetList' a i n@ gets the sequential list of @n@ items from array @a@ starting at index @i@.  Does not check bounds.
mxArrayGetList :: A.MXArrayComponent a => MXArray a -> MIndex -> Int -> ZIO r MatlabException [a]
mxArrayGetList a i n = do
  o <- mIndexOffset a i
  n <- if n == -1 then subtract o =.< mxArrayLength a else pure n
  mxArrayGetOffsetList a o n
-- |@'mxArraySetList' a i l@ sets the sequential items in array @a@ starting at index @i@ to @l@.  Does not check bounds.
mxArraySetList :: A.MXArrayComponent a => MXArray a -> MIndex -> [a] -> ZIO r MatlabException ()
mxArraySetList a i l = do
  o <- mIndexOffset a i
  mxArraySetOffsetList a o l

-- |Get a flat list of all elements in the array.
mxArrayGetAll :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException [a]
mxArrayGetAll a = mxArrayGetList a mStart (-1)

-- |Set a flat list of all elements in the array.
mxArraySetAll :: A.MXArrayComponent a => MXArray a -> [a] -> ZIO r MatlabException ()
mxArraySetAll a = mxArraySetList a mStart

mxArrayGetFirst :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException a
mxArrayGetFirst arr = mxArrayGetOffsetSafe arr 0

mxArrayGetLast :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException a
mxArrayGetLast arr = do
  arrLen <- mxArrayLength arr
  mxArrayGetOffsetSafe arr (arrLen - 1)

-- |Like mxArrayGetOffset but safe.
mxArrayGetOffsetSafe :: forall r a. A.MXArrayComponent a
  => MXArray a -> Int -> ZIO r MatlabException a
mxArrayGetOffsetSafe arr ix = ezlift $ EA.mxArrayGetOffsetSafe arr ix

-- | Create and populate an MXArray in one go. Named without 'mx' due to possible
-- | conformity to a typeclass function.
fromListIO :: (Foldable t, A.MXArrayComponent a)
  => t a -> ZIO r MatlabException (MXArray a)
fromListIO = ezlift . EA.fromListIO

-- | Like fromListIO but wraps elements in a cell. Most useful for converting a list of strings
-- | to a MATLAB cell array of strings. Named in conjunction with `fromListIO`, which is used
-- | as part of the implementation.
cellFromListsIO :: (Traversable s, Foldable t, A.MXArrayComponent a)
  => s (t a) -> ZIO r MatlabException (MXArray MCell)
cellFromListsIO = ezlift . EA.cellFromListsIO

-- | Extract all arrays of a given type from a Cell Array.
mxCellGetArraysOfType :: (A.MXArrayComponent a)
  => MXArray MCell -> ZIO r MatlabException ([MXArray a])
mxCellGetArraysOfType ca = do
  cellVals <- (fmap . fmap) mCell (mxArrayGetAll ca)
  sequence $ castMXArray <$> cellVals

-- | A convenience function to extract all arrays of a given type from a Cell Array;
-- | may have larger dimensions than the original Cell Array due to flattening.
mxCellGetAllOfType :: (A.MXArrayComponent a)
  => MXArray MCell -> ZIO r MatlabException [a]
mxCellGetAllOfType ca = join <$> mxCellGetAllListsOfType ca

mxCellGetAllListsOfType :: (A.MXArrayComponent a)
  => MXArray MCell -> ZIO r MatlabException [[a]]
mxCellGetAllListsOfType ca = do
  as <- mxCellGetArraysOfType ca
  traverse mxArrayGetAll as

-- |The class of standardly typeable array elements
-- class A.MXArrayComponent a where

-- |Determine whether the given array is of the correct type
isMXArray :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException Bool
-- |Create an array and initialize all its data elements to some default value, usually 0 or []
createMXArray :: A.MXArrayComponent a => MSize -> ZIO r MatlabException (MXArray a)

-- |Determine if an array is singleton. Equivalent to
--
-- >  liftM (all (1 ==)) . mxArraySize
isMXScalar :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException Bool

mxArrayGetOffset :: A.MXArrayComponent a => MXArray a -> Int -> ZIO r MatlabException a
mxArraySetOffset :: A.MXArrayComponent a => MXArray a -> Int -> a -> ZIO r MatlabException ()

mxArrayGetOffsetList :: A.MXArrayComponent a => MXArray a -> Int -> Int -> ZIO r MatlabException [a]
mxArraySetOffsetList :: A.MXArrayComponent a => MXArray a -> Int -> [a] -> ZIO r MatlabException ()

-- |Get the value of the first data element in an array or, more specifically, the value that the array will be interpreted as in scalar context
mxScalarGet :: A.MXArrayComponent a => MXArray a -> ZIO r MatlabException a

-- |Create a singleton (scalar) array having the specified value
createMXScalar :: A.MXArrayComponent a => a -> ZIO r MatlabException (MXArray a)
-- |Create a column vector from the given list.
createColVector :: A.MXArrayComponent a => [a] -> ZIO r MatlabException (MXArray a)
-- |Create a row vector from the given list.
createRowVector :: A.MXArrayComponent a => [a] -> ZIO r MatlabException (MXArray a)

createColVector l = do
  a <- createMXArray [length l]
  mxArraySetOffsetList a 0 l
  pure a

-- instance A.MXArrayComponent a => A.MXArrayComponent a where
isMXArray = ezlift . EA.isMXArray
createMXArray = ezlift . EA.createMXArray
createMXScalar = ezlift . EA.createMXScalar
isMXScalar = ezlift . EA.isMXScalar
mxScalarGet = ezlift . EA.mxScalarGet
createRowVector = ezlift . EA.createRowVector
mxArrayGetOffset a o = ezlift $ EA.mxArrayGetOffset a o
mxArraySetOffset a o mcv = ezlift $ EA.mxArraySetOffset a o mcv
mxArrayGetOffsetList a o n = ezlift $ EA.mxArrayGetOffsetList a o n
mxArraySetOffsetList a o v = ezlift $ EA.mxArraySetOffsetList a o v

createStruct :: MSize -> [String] -> ZIO r MatlabException MStructArray
createStruct s f = ezlift $ EA.createStruct s f

-- |Get the names of the fields
mStructFields :: MStructArray -> ZIO r MatlabException [String]
mStructFields = ezlift . EA.mStructFields

-- |Return the contents of the named field for the given element.
-- |Returns 'MNullArray' on no such field or if the field itself is NULL
mStructGet :: MStructArray -> MIndex -> String -> ZIO r MatlabException MAnyArray
mStructGet a i f = ezlift $ EA.mStructGet a i f

-- |Sets the contents of the named field for the given element. The input is stored in the array -- no copy is made.
mStructSet :: MStructArray -> MIndex -> String -> MXArray a -> ZIO r MatlabException ()
mStructSet a i f v = ezlift $ EA.mStructSet a i f v

-- |Add a field to a structure array.
mStructAddField :: MStructArray -> String -> ZIO r MatlabException ()
mStructAddField a f = ezlift $ EA.mStructAddField a f

-- |Remove a field from a structure array. Does nothing if no such field exists.
mStructRemoveField :: MStructArray -> String -> ZIO r MatlabException ()
mStructRemoveField a f = ezlift $ EA.mStructRemoveField a f

-- |Set the fields of a struct index to the given value list.  The list corresponds to the field list and must match in size.
mStructSetFields :: MStructArray -> MIndex -> [MXArray a] -> ZIO r MatlabException ()
mStructSetFields a i v = ezlift $ EA.mStructSetFields a i v

mObjectGetClass :: MStructArray -> ZIO r MatlabException (Maybe String)
mObjectGetClass = ezlift . EA.mObjectGetClass

-- |Set classname of an unvalidated object array.  It is illegal to call this function on a previously validated object array.
mObjectSetClass :: MStructArray -> String -> ZIO r MatlabException ()
mObjectSetClass a c = ezlift $ EA.mObjectSetClass a c

mxArrayIsComplex :: A.MXArrayComponent (MComplex a)
  => MXArray (MComplex a) -> ZIO r MatlabException Bool
mxArrayIsComplex = ezlift . EA.isMXArray

