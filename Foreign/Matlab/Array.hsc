{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
{-|
  Array access, including cell arrays and structures.

  Functions here are primarily thin wrappers to the underlying Matlab functions, and the same memory-management semantics hold.
  In particular, created arrays must be freed, 'copyMXArray' and 'freeMXArray' are deep operations, other set operations do not make copies.
-}

module Foreign.Matlab.Array (
    -- * Array manipulation
    anyMXArray,
    MNullArray, castMNull,
    mxArrayClass,
    mxArrayIsComplex,
    mxArraySize,
    mxArraySetSize,
    mxArrayLength,
    freeMXArray,
    copyMXArray,
    mIndexOffset,

    -- * Array element access
    MXArrayComponent(..),
    castMXArray,
    -- | array element access
    mxArrayGet, mxArraySet,
    -- | array list access
    mxArrayGetList, mxArraySetList,
    mxArrayGetAll, mxArraySetAll,
    mxArrayGetOffsetSafe, mxArrayGetFirst, mxArrayGetLast,
    -- mxArrayGetSafe, --TODO--
    fromListIO, cellFromListsIO,
    isMNull,

    -- * Struct access
    -- |Structs in Matlab are always arrays, and so can be accessed using most array accessors.
    -- |However, the modification functions such as 'mxArraySet' are not implemented because they could disrupt field data in the entire struct array, and so some specialized functions are necessary.
    MStructArray,
    createStruct,
    mStructFields,
    mStructGet, mStructSet,
    mStructSetFields,
    mStructAddField, mStructRemoveField,
    mxCellGetAllOfType, mxCellGetAllListsOfType, mxCellGetArraysOfType,

    -- ** Object access
    -- |Some structs are also validated (blessed) user objects.
    mObjectGetClass, mObjectSetClass
  ) where

import           Control.Monad
import           Data.Foldable (toList)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Data.Maybe (catMaybes)
import           Foreign.Matlab.Array.Internal
import           Foreign.Matlab.Util
import           Foreign.Matlab.Internal
import           Foreign.Matlab.Types

#include <matrix.h>

-- |(Un)cast an array to a generic type.
anyMXArray :: MXArray a -> MAnyArray
anyMXArray a = unsafeCastMXArray a

-- |A NULL (empty) array
type MNullArray = MXArray MNull
-- |Safely cast a generic array to a NULL array, or return Nothing if the array is not NULL
castMNull :: MAnyArray -> MIO (Maybe MNullArray)
castMNull a
  | isMNull a = pure $ Just (unsafeCastMXArray a)
  | otherwise = pure Nothing

foreign import ccall unsafe mxGetClassID :: MXArrayPtr -> IO MXClassID
-- |Return the representation of the type of the elements of an array
mxArrayClass :: MXArray a -> IO MXClass
mxArrayClass a
  | isMNull a = pure $ MXClassNull
  | otherwise = withMXArray a mxGetClassID >.= mx2hs

foreign import ccall unsafe mxGetNumberOfDimensions :: MXArrayPtr -> IO MWSize
foreign import ccall unsafe mxGetDimensions :: MXArrayPtr -> IO (Ptr MWSize)
-- |Get the size (dimensions) of an array
mxArraySize :: MXArray a -> MIO MSize
mxArraySize a = withMXArray a $ \a -> do
  n <- mxGetNumberOfDimensions a
  s <- mxGetDimensions a
  ndims n s

foreign import ccall unsafe mxSetDimensions :: MXArrayPtr -> Ptr MWSize -> MWSize -> IO CInt
-- |Set dimension array and number of dimensions
mxArraySetSize :: MXArray a -> MSize -> IO ()
mxArraySetSize a s = do
  r <- withMXArray a (\a -> withNDims s (\(nd,d) -> mxSetDimensions a d nd))
  when (r /= 0) $ fail "mxArraySetSize"

foreign import ccall unsafe mxGetNumberOfElements :: MXArrayPtr -> IO CSize
-- |Like `numel` in MATLAB.
mxArrayLength :: MXArray a -> MIO Int
mxArrayLength a = ii =.< withMXArray a mxGetNumberOfElements

foreign import ccall unsafe mxCalcSingleSubscript :: MXArrayPtr -> MWSize -> Ptr MWIndex -> IO MWIndex
-- |Convert an array subscript into an offset
mIndexOffset :: MXArray a -> MIndex -> MIO Int
mIndexOffset _ (MSubs []) = pure 0
mIndexOffset _ (MSubs [i]) = pure i
mIndexOffset a (MSubs i) = ii =.< withMXArray a (withNSubs i . uncurry . mxCalcSingleSubscript)

foreign import ccall unsafe mxDuplicateArray :: MXArrayPtr -> IO MXArrayPtr
-- |Make a deep copy of an array
copyMXArray :: MXArray a -> MIO (MXArray a)
copyMXArray a = withMXArray a mxDuplicateArray >>= mkMXArray

foreign import ccall unsafe mxDestroyArray :: MXArrayPtr -> IO ()
-- |Destroy an array and all of its contents.
freeMXArray :: MXArray a -> MIO ()
freeMXArray a = do
  withMXArray a mxDestroyArray
  mxArraySetSize a [0, 0]

-- | Create and populate an MXArray in one go. Named without 'mx' due to possible
-- | conformity to a typeclass function.
fromListIO :: (Foldable t, MXArrayComponent a) => t a -> MIO (MXArray a)
fromListIO = createRowVector . toList

-- | Like fromListIO but wraps elements in a cell. Most useful for converting a list of strings
-- | to a MATLAB cell array of strings. Named in conjunction with `fromListIO`, which is used
-- | as part of the implementation.
cellFromListsIO :: (Traversable s, Foldable t, MXArrayComponent a) => s (t a) -> MIO (MXArray MCell)
cellFromListsIO xss = do
  listOfStructArrays <- sequence $ fromListIO <$> xss
  arr <- createMXArray [length xss]
  mxArraySetAll arr (toList $ (MCell . anyMXArray) <$> listOfStructArrays)
  pure arr

-- |The class of standardly typeable array elements
class MXArrayComponent a where
  -- |Determine whether the given array is of the correct type
  isMXArray :: MXArray a -> MIO Bool
  -- |Create an array and initialize all its data elements to some default value, usually 0 or []
  createMXArray :: MSize -> MIO (MXArray a)

  -- |Determine if an array is singleton. Equivalent to
  --
  -- >  liftM (all (1 ==)) . mxArraySize
  isMXScalar :: MXArray a -> MIO Bool

  mxArrayGetOffset :: MXArray a -> Int -> MIO a
  mxArraySetOffset :: MXArray a -> Int -> a -> MIO ()

  mxArrayGetOffsetList :: MXArray a -> Int -> Int -> MIO [a]
  mxArraySetOffsetList :: MXArray a -> Int -> [a] -> MIO ()

  -- |Get the value of the first data element in an array or, more specifically, the value that the array will be interpreted as in scalar context
  mxScalarGet :: MXArray a -> MIO a

  -- |Create a singleton (scalar) array having the specified value
  createMXScalar :: a -> MIO (MXArray a)
  -- |Create a column vector from the given list.
  createColVector :: [a] -> MIO (MXArray a)
  -- |Create a row vector from the given list.
  createRowVector :: [a] -> MIO (MXArray a)

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

-- |Get the value of the specified array element.  Does not check bounds.
mxArrayGet :: MXArrayComponent a => MXArray a -> MIndex -> MIO a
mxArrayGet a i = mIndexOffset a i >>= mxArrayGetOffset a
-- |Set an element in an array to the specified value.  Does not check bounds.
mxArraySet :: MXArrayComponent a => MXArray a -> MIndex -> a -> MIO ()
mxArraySet a i v = do
  o <- mIndexOffset a i
  mxArraySetOffset a o v

-- |@'mxArrayGetList' a i n@ gets the sequential list of @n@ items from array @a@ starting at index @i@.  Does not check bounds.
mxArrayGetList :: MXArrayComponent a => MXArray a -> MIndex -> Int -> MIO [a]
mxArrayGetList a i n = do
  o <- mIndexOffset a i
  n <- if n == -1 then subtract o =.< mxArrayLength a else pure n
  mxArrayGetOffsetList a o n
-- |@'mxArraySetList' a i l@ sets the sequential items in array @a@ starting at index @i@ to @l@.  Does not check bounds.
mxArraySetList :: MXArrayComponent a => MXArray a -> MIndex -> [a] -> MIO ()
mxArraySetList a i l = do
  o <- mIndexOffset a i
  mxArraySetOffsetList a o l

-- |Get a flat list of all elements in the array.
mxArrayGetAll :: MXArrayComponent a => MXArray a -> IO [a]
mxArrayGetAll a = mxArrayGetList a mStart (-1)

-- |Set a flat list of all elements in the array.
mxArraySetAll :: MXArrayComponent a => MXArray a -> [a] -> IO ()
mxArraySetAll a = mxArraySetList a mStart

mxArrayGetFirst :: MXArrayComponent a => MXArray a -> MIO (Either String a)
mxArrayGetFirst arr = mxArrayGetOffsetSafe arr 0

mxArrayGetLast :: MXArrayComponent a => MXArray a -> MIO (Either String a)
mxArrayGetLast arr = do
  arrLen <- mxArrayLength arr
  mxArrayGetOffsetSafe arr (arrLen - 1)

-- |Like mxArrayGetOffset but safe.
mxArrayGetOffsetSafe :: forall a. MXArrayComponent a => MXArray a -> Int -> MIO (Either String a)
mxArrayGetOffsetSafe arr ix
  | isMNull arr = pure $ Left "Couldn't get element of null array"
  | otherwise = do
    arrLen <- mxArrayLength arr
    safeGetElem arrLen ix
  where
    safeGetElem :: Int -> Int -> MIO (Either String a)
    safeGetElem aLen aIx
      | (aIx < aLen) && (aLen > 0) = Right <$> mxArrayGetOffset arr aIx
      | otherwise = pure $ Left $ "Couldn't get element at index "
        <> (show aIx) <> " of " <> (show aLen) <> "-length array"

-- |Safely cast a generic array to a type, or return Nothing if the array does not have the proper type
castMXArray :: forall a. MXArrayComponent a => MAnyArray -> MIO (Maybe (MXArray a))
castMXArray a
  | isMNull a = pure Nothing
  | otherwise = do
      y <- isMXArray b
      pure $ if y then Just b else Nothing
      where
        b :: MXArray a
        b = unsafeCastMXArray a

-- | Extract all arrays of a given type from a Cell Array.
mxCellGetArraysOfType :: MXArrayComponent a => MXArray MCell -> MIO ([MXArray a])
mxCellGetArraysOfType ca = do
  cellVals <- (fmap . fmap) mCell (mxArrayGetAll ca)
  mxaMays :: [Maybe (MXArray a)] <- sequence $ castMXArray <$> cellVals
  pure $ catMaybes mxaMays

-- | A convenience function to extract all arrays of a given type from a Cell Array;
-- | may have larger dimensions than the original Cell Array due to flattening.
mxCellGetAllOfType :: MXArrayComponent a => MXArray MCell -> MIO [a]
mxCellGetAllOfType ca = join <$> mxCellGetAllListsOfType ca

mxCellGetAllListsOfType :: (MXArrayComponent a)
  => MXArray MCell -> MIO [[a]]
mxCellGetAllListsOfType ca = do
  as <- mxCellGetArraysOfType ca
  traverse mxArrayGetAll as

class (MXArrayComponent a, MType mx a, Storable mx) => MXArrayData mx a where
  -- withArrayData :: MXArray a -> (Ptr mx -> IO b) -> IO b
  -- withArrayData = withArrayDataDef

  -- withArrayDataOff :: MXArray a -> Int -> (Ptr mx -> IO b) -> IO b
  -- withArrayDataOff = withArrayDataOffDef

  arrayDataGet :: MXArray a -> Int -> IO a
  arrayDataGet = arrayDataGetDef

  arrayDataSet :: MXArray a -> Int -> a -> IO ()
  arrayDataSet = arrayDataSetDef

  arrayDataGetList :: MXArray a -> Int -> Int -> IO [a]
  arrayDataGetList = arrayDataGetListDef
  
  arrayDataSetList :: MXArray a -> Int -> [a] -> IO ()
  arrayDataSetList = arrayDataSetListDef

#let arrayDataComponent = "\
mxArrayGetOffset = arrayDataGet ;\
mxArraySetOffset = arrayDataSet ;\
mxArrayGetOffsetList = arrayDataGetList ;\
mxArraySetOffsetList = arrayDataSetList\
"
--"

instance MXArrayComponent MLogical where
  isMXArray = isMXArrayMLogical
  createMXArray = createMXArrayMLogical
  createMXScalar = createMXScalarMLogical
  isMXScalar = isMXScalarMLogical
  mxScalarGet = mxScalarGetMLogical
  #arrayDataComponent
instance MXArrayData MXLogical MLogical -- where
--  withArrayData = withArrayDataMLogical

instance MXArrayComponent MChar where
  isMXArray = isMXArrayMChar
  createMXArray = createMXArrayMChar
  createRowVector = createRowVectorMChar
  #arrayDataComponent
instance MXArrayData MXChar MChar -- where
--  withArrayData = withArrayDataMChar


#let numarray t = "\
instance MXArrayComponent M%s where\n\
  isMXArray a = boolC =.< withMXArray a mxIs%s\n\
  createMXArray s = withNDims s (uncurry $ createNumericArray (mxClassOf (undefined :: M%s)) False) >>= mkMXArray\n\
  \
mxArrayGetOffset = arrayDataGet ;\
mxArraySetOffset = arrayDataSet ;\
mxArrayGetOffsetList = arrayDataGetList ;\
mxArraySetOffsetList = arrayDataSetList\
  \n\
instance MXArrayData MX%s M%s\
", #t, #t, #t, #t, #t

instance MXArrayComponent MDouble where
  isMXArray = isMXArrayMDouble
  createMXScalar = createMXScalarMDouble
  mxScalarGet = mxScalarGetMDouble
  createMXArray = createMXArrayMDouble
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
  isMXArray = isMXArrayMCell
  createMXArray = createMXArrayMCell
  -- Get a the specified cell element (not a copy).
  mxArrayGetOffset = mxArrayGetOffsetMCell
  -- Set an element in a cell array to the specified value. The cell takes ownership of the array: and no copy is made. Any existing value should be freed first.
  mxArraySetOffset = mxArraySetOffsetMCell

-- |Return the contents of the named field for the given element.
-- |Returns 'MNullArray' on no such field or if the field itself is NULL
mStructGet :: MStructArray -> MIndex -> String -> MIO MAnyArray
-- |Sets the contents of the named field for the given element. The input is stored in the array -- no copy is made.
mStructSet :: MStructArray -> MIndex -> String -> MXArray a -> MIO ()
mStructGet a i f = do
  o <- mIndexOffset a i
  withMXArray a (\a -> withCString f (mxGetField a (ii o) >=> mkMXArray))
mStructSet a i f v = do
  o <- mIndexOffset a i
  withMXArray a (\a -> withCString f (withMXArray v . mxSetField a (ii o)))

-- |Add a field to a structure array.
mStructAddField :: MStructArray -> String -> MIO ()
-- |Remove a field from a structure array. Does nothing if no such field exists.
mStructRemoveField :: MStructArray -> String -> MIO ()
mStructAddField a f = do
  i <- withMXArray a (withCString f . mxAddField)
  when (i < 0) $ fail "mxAddField"
mStructRemoveField a f = withMXArray a $ \a -> do
  i <- withCString f (mxGetFieldNumber a)
  if i < 0
    then fail "mxRemoveField"
    else mxRemoveField a i

-- |Set the fields of a struct index to the given value list.  The list corresponds to the field list and must match in size.
mStructSetFields :: MStructArray -> MIndex -> [MXArray a] -> MIO ()
mStructSetFields a i v = do
  o <- mIndexOffset a i
  withMXArray a (\a -> zipWithM_ (\v -> withMXArray v . mxSetFieldByNumber a (ii o)) v [0..])

instance MXArrayComponent MStruct where
  isMXArray = isMXArrayMStruct
  createMXArray = createMXArrayMStruct
  mxArrayGetOffset = mxArrayGetOffsetMStruct
  mxArraySetOffset = mxArraySetOffsetMStruct
  mxArrayGetOffsetList = mxArrayGetOffsetListMStruct
  createMXScalar = createMXScalarMStruct


-- |Determine if a struct array is a user defined object, and return its class name, if any.
mObjectGetClass :: MStructArray -> IO (Maybe String)
mObjectGetClass a = do
  b <- boolC =.< withMXArray a mxIsObject
  if b
    then Just =.< withMXArray a (mxGetClassName >=> peekCString)
    else pure Nothing

-- |Set classname of an unvalidated object array.  It is illegal to call this function on a previously validated object array.
mObjectSetClass :: MStructArray -> String -> IO ()
mObjectSetClass a c = do
  r <- withMXArray a (withCString c . mxSetClassName)
  when (r /= 0) $ fail "mObjectSetClass"

-- |Complex array access.
instance (RealFloat a, MNumeric a, MXArrayData mx a) => MXArrayComponent (MComplex a) where
  isMXArray = isMXArrayMComplex
  createMXArray = createMXArrayMComplex
  mxArrayGetOffset = mxArrayGetOffsetMComplex
  mxArraySetOffset = mxArraySetOffsetMComplex
  mxArrayGetOffsetList = mxArrayGetOffsetListMComplex
  mxArraySetOffsetList = mxArraySetOffsetListMComplex

isMXArrayMComplex :: (RealFloat a, MType mx a, Storable mx, MXArrayComponent a)
  => MXArray (MComplex a) -> IO Bool
isMXArrayMComplex a = liftM2 (&&) (isMXArray (castReal a)) (mxArrayIsComplex a)
