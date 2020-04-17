{-|
  Mapping of basic Matlab types to Haskell types.
-}
module Foreign.Matlab.Types (
    -- * Representations of Matlab types
    -- |Most types are simple aliases of Haskell types, given simply to identify them and provide a consistent naming scheme.

    MIO,

    MXClass(..),
    MChar,
    MLogical,
    MScalar,
    MType,

    MNumeric,
    -- * Numeric types
    MDouble,
    MSingle,
    MInt8,
    MInt16,
    MInt32,
    MInt64,
    MUint8,
    MUint16,
    MUint32,
    MUint64,
    MComplex,

    -- * Array indexing
    MSize, MSubs,
    MIndex(..),
    mStart, mOffset,
    mSizeRange, mRangeSize,
    normMSize, realMSize,

    -- * Opaque types
    MXArray,
    MAnyArray,
    MCell(MCell), mCell,
    MStruct,
    MFun
  ) where

import Data.Complex
import Data.Ix
import Foreign.Matlab.Internal

-- |The class of types which are simple Matlab scalars and can be array elements
class MScalar a
-- |The class of types which Matlab consisters \"numeric\"
class (MScalar a, Num a) => MNumeric a

instance MScalar MChar
instance MScalar MLogical
instance MScalar MDouble
instance MScalar MSingle
instance MScalar MInt8
instance MScalar MInt16
instance MScalar MInt32
instance MScalar MInt64
instance MScalar MUint8
instance MScalar MUint16
instance MScalar MUint32
instance MScalar MUint64
instance MNumeric a => MScalar (MComplex a)

instance MNumeric MDouble
instance MNumeric MSingle
instance MNumeric MInt8
instance MNumeric MInt16
instance MNumeric MInt32
instance MNumeric MInt64
instance MNumeric MUint8
instance MNumeric MUint16
instance MNumeric MUint32
instance MNumeric MUint64

-- |Complex numeric types.  Unfortunately, 'Complex' only applies to 'RealFloat' types, whereas Matlab allows any numeric type, so some types are (currently) unaccessable.
type MComplex = Complex

-- |The type of array sizes, which are the column-major lengths of each dimensions
type MSize = [Int]
-- |The type of array index subscripts, which are the column-major, 0-based indices in each dimension with normal Matlab semantics (flatten along last dimension)
type MSubs = [Int]
-- |Ways to index an array.  Using Matlab semantics, a singleton MSubs [n] is equivalent to a raw 0-based offset (MOffset n).
newtype MIndex = MSubs MSubs
    -- MOffset Int -- ^0-based offset from the beginning of the array (equivalent to singleton MSubs)

-- |First index in array
mStart :: MIndex
mStart = MSubs []

-- |Raw, 0-based array offset index
mOffset :: Int -> MIndex
mOffset i = MSubs [i]

instance Show MIndex where
  showsPrec _ (MSubs []) = id
  showsPrec _ (MSubs l) = showChar '(' . foldr1 (\s r -> s . showChar ',' . r) (map shows l) . showChar ')'
instance Eq MIndex where
  MSubs a == MSubs b = eq a b where
    eq [] [] = True
    eq (x:a) (y:b) = (x == y) && eq a b
    eq [] b = eq [0] b
    eq a [] = eq a [0]
instance Ord MIndex where
  compare (MSubs a) (MSubs b) = cmp a b where
    cmp [] [] = EQ
    cmp (x:a) (y:b) = case cmp a b of { EQ -> compare x y ; r -> r }
    cmp [] b = cmp [0] b
    cmp a [] = cmp a [0]
instance Ix MIndex where
  range (MSubs a, MSubs b) = map MSubs $ rng a b where
    rng [] [] = [[]]
    rng (x:a) (y:b) = concatMap (\l -> map (:l) [x..y]) $ rng a b
    rng [] b = rng [0] b
    rng _ _ = error "MIndex.range: length mismatch"
  index (MSubs a, MSubs b) (MSubs l) = idx a b l where
    idx _ _ [] = 0
    idx (x:a) (y:b) (i:l) = i-x+((y-x+1)*idx a b l)
    idx [] [] (0:l) = idx [] [] l
    idx [] b l = idx [0] b l
    idx _ _ _ = error "MIndex.index: length mismatch"
  inRange (MSubs a, MSubs b) (MSubs l) = inr a b l where
    inr _ _ [] = True
    inr a@(x:_) b@(_:_) [i] = i >= x && i-x < rangeSize (MSubs a, MSubs b)
    inr (x:a) (y:b) (i:l) = i >= x && i <= y && inr a b l
    inr [] [] (0:l) = inr [] [] l
    inr [] b l = inr [0] b l
    inr _ _ _ = error "MIndex.inRange: length mismatch"
  rangeSize (MSubs a, MSubs b) = rsz a b where
    rsz [] b = product $ map succ b
    rsz (x:a) (y:b) = (y-x+1)*rsz a b
    rsz _ _ = error "MIndex.rangeSize: length mismatch"

-- |Convert an array size to an index range, which will be of the form ((0,0,0...),(i-1,j-1,k-1,...))
mSizeRange :: MSize -> (MIndex,MIndex)
mSizeRange l = (MSubs [], MSubs $ map pred $ normMSize l)

-- |Convert an index range to an array size
mRangeSize :: (MIndex,MIndex) -> MSize
mRangeSize (MSubs [], MSubs l) = map succ l
mRangeSize _ = error "mRangeSize: invalid lower bound"

-- |Get the size form that Matlab likes (@length (realMSize s) >= 2@)
realMSize :: MSize -> MSize
realMSize [] = [1,1]
realMSize [n] = [n,1]
realMSize s = s

-- |Get a more useful size form (no trailing singletons)
normMSize :: MSize -> MSize
normMSize [] = []
normMSize (1:l) = case normMSize l of { [] -> [] ; l -> (1:l) }
normMSize (x:l) = x : normMSize l
