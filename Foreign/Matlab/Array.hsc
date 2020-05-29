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
    MXArrayComponent (mxArrayGetOffset, mxArraySetOffset
      , mxArrayGetOffsetList, mxArraySetOffsetList
      , mxScalarGet, isMXScalar
      , createMXArray, createMXScalar
      , createColVector, createRowVector),
    castMXArray,
    -- | array element access
    mxArrayGet, mxArraySet,
    -- | array list access
    mxArrayGetList, mxArraySetList,
    mxArrayGetAll, mxArraySetAll,

    -- * Struct access
    -- |Structs in Matlab are always arrays, and so can be accessed using most array accessors.
    -- |However, the modification functions such as 'mxArraySet' are not implemented because they could disrupt field data in the entire struct array, and so some specialized functions are necessary.
    MStructArray,
    createStruct,
    mStructFields,
    mStructGet, mStructSet,
    mStructSetFields,
    mStructAddField, mStructRemoveField,
    mxCellGetAllOfType, mxCellGetArraysOfType,

    -- ** Object access
    -- |Some structs are also validated (blessed) user objects.
    mObjectGetClass, mObjectSetClass
  ) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.Complex
import Data.Maybe (catMaybes)
import Foreign.Matlab.Util
import Foreign.Matlab.Internal
import Foreign.Matlab.Types

#include <matrix.h>

-- |(Un)cast an array to a generic type.
anyMXArray :: MXArray a -> MAnyArray
anyMXArray a = unsafeCastMXArray a

-- |A NULL (empty) array
type MNullArray = MXArray MNull
-- |Safely cast a generic array to a NULL array, or return Nothing if the array is not NULL
castMNull :: MAnyArray -> MIO (Maybe MNullArray)
castMNull a
  | isMNull a = return $ Just (unsafeCastMXArray a)
  | otherwise = return Nothing

foreign import ccall unsafe mxGetClassID :: MXArrayPtr -> IO MXClassID
-- |Return the representation of the type of the elements of an array
mxArrayClass :: MXArray a -> IO MXClass
mxArrayClass a
  | isMNull a = return $ MXClassNull
  | otherwise = withMXArray a mxGetClassID >.= mx2hs

ndims :: MWSize -> Ptr MWSize -> IO MSize
ndims n s = map ii =.< peekArray (ii n) s
--nsubs = ndims

withNSubs :: With MSubs (MWSize, Ptr MWSize) (IO a)
withNSubs l f = withArrayLen (map ii l) (\l a -> f (ii l, a))
withNDims :: With MSize (MWSize, Ptr MWSize) (IO a)
withNDims = withNSubs . realMSize

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
mIndexOffset _ (MSubs []) = return 0
mIndexOffset _ (MSubs [i]) = return i
mIndexOffset a (MSubs i) = ii =.< withMXArray a (withNSubs i . uncurry . mxCalcSingleSubscript)

foreign import ccall unsafe mxDuplicateArray :: MXArrayPtr -> IO MXArrayPtr
-- |Make a deep copy of an array
copyMXArray :: MXArray a -> MIO (MXArray a)
copyMXArray a = withMXArray a mxDuplicateArray >>= mkMXArray

foreign import ccall unsafe mxDestroyArray :: MXArrayPtr -> IO ()
-- |Destroy an array and all of its contents.
freeMXArray :: MXArray a -> MIO ()
freeMXArray a = withMXArray a mxDestroyArray

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

  isMXArray _ = return False
  isMXScalar a = liftM2 (&&) (isMXArray a) (all (1 ==) =.< mxArraySize a)

  mxArrayGetOffsetList a o n = mapM (mxArrayGetOffset a) [o..o+n-1]
  mxArraySetOffsetList a o = zipWithM_ (mxArraySetOffset a . (o+)) [0..]

  mxScalarGet a = mxArrayGetOffset a 0

  createMXScalar x = do
    a <- createMXArray [1]
    mxArraySetOffset a 0 x
    return a
  createRowVector l = do
    a <- createMXArray [1,length l]
    mxArraySetOffsetList a 0 l
    return a
  createColVector l = do
    a <- createMXArray [length l]
    mxArraySetOffsetList a 0 l
    return a

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
  n <- if n == -1 then subtract o =.< mxArrayLength a else return n
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

-- |Safely cast a generic array to a type, or return Nothing if the array does not have the proper type
castMXArray :: forall a. MXArrayComponent a => MAnyArray -> MIO (Maybe (MXArray a))
castMXArray a
  | isMNull a = return Nothing
  | otherwise = do
      y <- isMXArray b
      return $ if y then Just b else Nothing
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
mxCellGetAllOfType ca = do
  as <- mxCellGetArraysOfType ca
  join <$> (sequence $ mxArrayGetAll <$> as)

foreign import ccall unsafe mxGetData :: MXArrayPtr -> IO (Ptr a)

class (MXArrayComponent a, MType mx a, Storable mx) => MXArrayData mx a where
  withArrayData :: MXArray a -> (Ptr mx -> IO b) -> IO b
  withArrayDataOff :: MXArray a -> Int -> (Ptr mx -> IO b) -> IO b
  arrayDataGet :: MXArray a -> Int -> IO a
  arrayDataSet :: MXArray a -> Int -> a -> IO ()

  arrayDataGetList :: MXArray a -> Int -> Int -> IO [a]
  arrayDataSetList :: MXArray a -> Int -> [a] -> IO ()

  withArrayData a f = withMXArray a (mxGetData >=> f)
  withArrayDataOff a o f = withArrayData a (\p -> f (advancePtr p o))
  arrayDataGet a o   = withArrayDataOff a o (mx2hs .=< peek)
  arrayDataSet a o v = withArrayDataOff a o (\p -> poke p (hs2mx v))
  arrayDataGetList a o n = withArrayDataOff a o (map mx2hs .=< peekArray n)
  arrayDataSetList a o l = withArrayDataOff a o (\p -> pokeArray p (map hs2mx l))
#let arrayDataComponent = "\
mxArrayGetOffset = arrayDataGet ;\
mxArraySetOffset = arrayDataSet ;\
mxArrayGetOffsetList = arrayDataGetList ;\
mxArraySetOffsetList = arrayDataSetList\
"
--"

foreign import ccall unsafe mxIsLogical :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateLogicalArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetLogicals :: MXArrayPtr -> IO (Ptr MXLogical)
foreign import ccall unsafe mxCreateLogicalScalar :: CBool -> IO MXArrayPtr
foreign import ccall unsafe mxIsLogicalScalar :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxIsLogicalScalarTrue :: MXArrayPtr -> IO CBool
instance MXArrayComponent MLogical where
  isMXArray a = boolC =.< withMXArray a mxIsLogical
  createMXArray s = withNDims s (uncurry mxCreateLogicalArray) >>= mkMXArray
  createMXScalar = mxCreateLogicalScalar . cBool >=> mkMXArray
  isMXScalar a = boolC =.< withMXArray a mxIsLogicalScalar
  mxScalarGet a = boolC =.< withMXArray a mxIsLogicalScalarTrue
  #arrayDataComponent
instance MXArrayData MXLogical MLogical where
  withArrayData a f = withMXArray a (mxGetLogicals >=> f)

foreign import ccall unsafe mxIsChar :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateCharArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetChars :: MXArrayPtr -> IO (Ptr MXChar)
foreign import ccall unsafe mxCreateStringFromNChars :: CString -> MWSize -> IO MXArrayPtr
instance MXArrayComponent MChar where
  isMXArray a = boolC =.< withMXArray a mxIsChar
  createMXArray s = withNDims s (uncurry mxCreateCharArray) >>= mkMXArray
  createRowVector s = 
    mkMXArray =<< withCStringLen s (\(s,n) -> mxCreateStringFromNChars s (ii n))
  #arrayDataComponent
instance MXArrayData MXChar MChar where
  withArrayData a f = withMXArray a (mxGetChars >=> f)

foreign import ccall unsafe mxCreateNumericArray :: MWSize -> Ptr MWSize -> MXClassID -> (#type mxComplexity) -> IO MXArrayPtr
createNumericArray :: MXClass -> Bool -> MWSize -> Ptr MWSize -> IO MXArrayPtr
createNumericArray t c n s = mxCreateNumericArray n s (hs2mx t) (if c then (#const mxCOMPLEX) else (#const mxREAL))

#let numarray t = "\
foreign import ccall unsafe mxIs%s :: MXArrayPtr -> IO CBool\n\
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
", #t, #t, #t, #t, #t, #t

foreign import ccall unsafe mxIsDouble :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateDoubleScalar :: MXDouble -> IO MXArrayPtr
foreign import ccall unsafe mxGetScalar :: MXArrayPtr -> IO MXDouble
instance MXArrayComponent MDouble where
  isMXArray a = boolC =.< withMXArray a mxIsDouble
  createMXScalar = mxCreateDoubleScalar . hs2mx >=> mkMXArray
  mxScalarGet a = withMXArray a mxGetScalar
  createMXArray s = withNDims s (uncurry $ createNumericArray (mxClassOf (undefined :: Double)) False) >>= mkMXArray
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


foreign import ccall unsafe mxIsCell :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxCreateCellArray :: MWSize -> Ptr MWSize -> IO MXArrayPtr
foreign import ccall unsafe mxGetCell :: MXArrayPtr -> MWIndex -> IO MXArrayPtr
foreign import ccall unsafe mxSetCell :: MXArrayPtr -> MWIndex -> MXArrayPtr -> IO ()
instance MXArrayComponent MCell where
  isMXArray a = boolC =.< withMXArray a mxIsCell
  createMXArray s = withNDims s (uncurry mxCreateCellArray) >>= mkMXArray
  -- Get a the specified cell element (not a copy).
  mxArrayGetOffset a o = withMXArray a (\a -> mxGetCell a (ii o) >>= mkMXArray >.= MCell)
  -- Set an element in a cell array to the specified value. The cell takes ownership of the array: and no copy is made. Any existing value should be freed first.
  mxArraySetOffset a o (MCell v) = withMXArray a (\a -> withMXArray v (mxSetCell a (ii o)))

-- |A (array of) structs
type MStructArray = MXArray MStruct

foreign import ccall unsafe mxIsStruct :: MXArrayPtr -> IO CBool
foreign import ccall unsafe mxIsObject :: MXArrayPtr -> IO CBool

foreign import ccall unsafe mxCreateStructArray :: MWSize -> Ptr MWSize -> CInt -> Ptr CString -> IO MXArrayPtr
-- |Create an N-Dimensional structure array having the specified fields; initialize all values to 'MNullArray'
createStruct :: MSize -> [String] -> MIO MStructArray
createStruct s f =
  withNDims s (\(nd,d) ->
    mapWithArrayLen withCString f (\(f,nf) ->
      mxCreateStructArray nd d (ii nf) f))
  >>= mkMXArray

foreign import ccall unsafe mxGetNumberOfFields :: MXArrayPtr -> IO CInt
foreign import ccall unsafe mxGetFieldNameByNumber :: MXArrayPtr -> CInt -> IO CString
foreign import ccall unsafe mxGetFieldNumber :: MXArrayPtr -> CString -> IO CInt
-- |Get the names of the fields
mStructFields :: MStructArray -> MIO [String]
mStructFields a = withMXArray a $ \a -> do
  n <- mxGetNumberOfFields a
  forM [0..pred n] (mxGetFieldNameByNumber a >=> peekCString)

foreign import ccall unsafe mxGetField :: MXArrayPtr -> MWIndex -> CString -> IO MXArrayPtr
foreign import ccall unsafe mxSetField :: MXArrayPtr -> MWIndex -> CString -> MXArrayPtr -> IO ()
foreign import ccall unsafe mxGetFieldByNumber :: MXArrayPtr -> MWIndex -> CInt -> IO MXArrayPtr
foreign import ccall unsafe mxSetFieldByNumber :: MXArrayPtr -> MWIndex -> CInt -> MXArrayPtr -> IO ()

-- |Return the contents of the named field for the given element. Returns 'MNullArray' on no such field or if the field itself is NULL
mStructGet :: MStructArray -> MIndex -> String -> MIO MAnyArray
-- |Sets the contents of the named field for the given element. The input is stored in the array -- no copy is made.
mStructSet :: MStructArray -> MIndex -> String -> MXArray a -> MIO ()
mStructGet a i f = do
  o <- mIndexOffset a i
  withMXArray a (\a -> withCString f (mxGetField a (ii o) >=> mkMXArray))
mStructSet a i f v = do
  o <- mIndexOffset a i
  withMXArray a (\a -> withCString f (withMXArray v . mxSetField a (ii o)))

foreign import ccall unsafe mxAddField :: MXArrayPtr -> CString -> IO CInt
foreign import ccall unsafe mxRemoveField :: MXArrayPtr -> CInt -> IO ()
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

structGetOffsetFields :: MStructArray -> [String] -> Int -> IO MStruct
structGetOffsetFields a f o =
  MStruct =.< withMXArray a (\a -> zipWithM (\f -> ((,) f) .=< (mxGetFieldByNumber a (ii o) >=> mkMXArray)) f [0..])

-- |Set the fields of a struct index to the given value list.  The list corresponds to the field list and must match in size.
mStructSetFields :: MStructArray -> MIndex -> [MXArray a] -> MIO ()
mStructSetFields a i v = do
  o <- mIndexOffset a i
  withMXArray a (\a -> zipWithM_ (\v -> withMXArray v . mxSetFieldByNumber a (ii o)) v [0..])

instance MXArrayComponent MStruct where
  isMXArray a = liftM2 (||) (boolC =.< withMXArray a mxIsStruct) (boolC =.< withMXArray a mxIsObject)
  createMXArray s = createStruct s []
  mxArrayGetOffset a o = do
    f <- mStructFields a
    structGetOffsetFields a f o
  mxArraySetOffset = error "mxArraySet undefined for MStruct: use mStructSet"
  mxArrayGetOffsetList a o n = do
    f <- mStructFields a
    mapM (structGetOffsetFields a f) [o..o+n-1]
  createMXScalar (MStruct fv) = do
    a <- createStruct [1] f
    withMXArray a $ \a -> zipWithM_ (\i v -> withMXArray v (mxSetFieldByNumber a 0 i)) [0..] v
    return a
    where
      (f,v) = unzip fv

foreign import ccall unsafe mxGetClassName :: MXArrayPtr -> IO CString
-- |Determine if a struct array is a user defined object, and return its class name, if any.
mObjectGetClass :: MStructArray -> IO (Maybe String)
mObjectGetClass a = do
  b <- boolC =.< withMXArray a mxIsObject
  if b
    then Just =.< withMXArray a (mxGetClassName >=> peekCString)
    else return Nothing

foreign import ccall unsafe mxSetClassName :: MXArrayPtr -> CString -> IO CInt
-- |Set classname of an unvalidated object array.  It is illegal to call this function on a previously validated object array.
mObjectSetClass :: MStructArray -> String -> IO ()
mObjectSetClass a c = do
  r <- withMXArray a (withCString c . mxSetClassName)
  when (r /= 0) $ fail "mObjectSetClass"

castReal :: MXArray (Complex a) -> MXArray a
castReal = unsafeCastMXArray

foreign import ccall unsafe mxGetImagData :: MXArrayPtr -> IO (Ptr a)
withRealDataOff :: MXArrayData mx a => MXArray (Complex a) -> Int -> (Ptr mx -> IO b) -> IO b
withRealDataOff = withArrayDataOff . castReal
withImagDataOff :: MXArrayData mx a => MXArray (Complex a) -> Int -> (Ptr mx -> IO b) -> IO b
withImagDataOff a o f = withMXArray a (mxGetImagData >=> \p -> f (advancePtr p o))

foreign import ccall unsafe mxIsComplex :: MXArrayPtr -> IO CBool
mxArrayIsComplex :: MXArray a -> IO Bool
mxArrayIsComplex a = boolC =.< withMXArray a mxIsComplex

-- |Complex array access.
instance (RealFloat a, MNumeric a, MXArrayData mx a) => MXArrayComponent (MComplex a) where
  isMXArray a = liftM2 (&&) (isMXArray (castReal a)) (mxArrayIsComplex a)
  createMXArray s = withNDims s (uncurry $ createNumericArray (mxClassOf (undefined :: a)) True) >>= mkMXArray

  mxArrayGetOffset a o = do
    r <- withRealDataOff a o (mx2hs .=< peek)
    c <- withImagDataOff a o (mx2hs .=< peek)
    return $ r :+ c
  mxArraySetOffset a o (r :+ c) = do
    withRealDataOff a o (\p -> poke p (hs2mx r))
    withImagDataOff a o (\p -> poke p (hs2mx c))
  mxArrayGetOffsetList a o n = do
    r <- withRealDataOff a o (map mx2hs .=< peekArray n)
    c <- withImagDataOff a o (map mx2hs .=< peekArray n)
    return $ zipWith (:+) r c
  mxArraySetOffsetList a o v = do
    withRealDataOff a o (\p -> pokeArray p (map hs2mx r))
    withImagDataOff a o (\p -> pokeArray p (map hs2mx c))
    where (r,c) = unzip $ map (\(r:+c) -> (r,c)) v
