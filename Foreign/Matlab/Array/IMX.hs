{-|
  Safe immutable intermediate (functional) Matlab data structures.

  This provides an alternative representation of Matlab data structures which can be marshalled to and from 'MXArray'.
  An instance of 'Show' is provided which creates Matlab expressions.
-}
module Foreign.Matlab.Array.IMX (
    IMXData(..),
    IMXArrayElem,

    -- * Interface with "Matlab.Array"
    imxData, iMXData,
    imxFun, iMXFun,
    withIMXData, takeIMXData,
    withIMXDataList,

    -- * Construction and access
    imxSize,
    imxConstr, imxArray,
    listIMX, scalarIMX,
    imxList, imxScalar,
    listIMXStruct,
    imxStructList
  ) where

import Control.Monad
import Data.Array.IArray
import Data.Complex
import Data.List
import Text.Show
import Foreign.Matlab.Util
import Foreign.Matlab.Internal
import Foreign.Matlab.Types
import Foreign.Matlab.Array

type IMXArray a = Array MIndex a
-- |The basic immutable (functional) representation of Matlab data structures, representing a generic 'MXArray'
data IMXData =
    IMXNull
  | IMXCell	(IMXArray IMXData)
  | IMXStruct   [String] (Array (MIndex,Int) IMXData) -- ^ field name list and array mapping (index,field index) to values
  | IMXLogical	(IMXArray MLogical)
  | IMXChar	(IMXArray MChar)
  | IMXDouble	(IMXArray MDouble)
  | IMXSingle	(IMXArray MSingle)
  | IMXInt8	(IMXArray MInt8)
  | IMXUint8	(IMXArray MUint8)
  | IMXInt16	(IMXArray MInt16)
  | IMXUint16	(IMXArray MUint16)
  | IMXInt32	(IMXArray MInt32)
  | IMXUint32	(IMXArray MUint32)
  | IMXInt64	(IMXArray MInt64)
  | IMXUint64	(IMXArray MUint64)
  | IMXComplexDouble	(IMXArray (MComplex MDouble))
  | IMXComplexSingle	(IMXArray (MComplex MSingle))
  | IMXObject   String IMXData -- ^ object class name and object data, currently always IMXStruct
  deriving (Eq)

type IMXFun = [IMXData] -> Int -> IO [IMXData]

fixMSize :: MSize -> [a] -> MSize
fixMSize s l = maybe s (\i -> replaceIndex s i (length l `div` negate (product s))) $ elemIndex (-1) s

-- |Create an sized array for 'IMXData' from a sequential list of elements.  'MSize' may contain at most one -1 value, which will be inferred from the length of the list.
listIMXArray :: MSize -> [e] -> IMXArray e
listIMXArray s l = listArray (mSizeRange (fixMSize s l)) l

-- |Create a scalar (array) for 'IMXData'
scalarIMXArray :: e -> IMXArray e
scalarIMXArray e = listIMXArray [] [e]

class IMXArrayElem a where
  -- |Generic 'IMXData' 'IMXArray' constructor
  imxConstr :: IMXArray a -> IMXData
  -- |Generic 'IMXData' 'IMXArray' accessor
  imxArray :: IMXData -> Maybe (IMXArray a)
instance IMXArrayElem IMXData  where { imxConstr = IMXCell    ; imxArray (IMXCell    a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MLogical where { imxConstr = IMXLogical ; imxArray (IMXLogical a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MChar    where { imxConstr = IMXChar    ; imxArray (IMXChar    a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MDouble  where { imxConstr = IMXDouble  ; imxArray (IMXDouble  a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MSingle  where { imxConstr = IMXSingle  ; imxArray (IMXSingle  a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MInt8    where { imxConstr = IMXInt8    ; imxArray (IMXInt8    a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MUint8   where { imxConstr = IMXUint8   ; imxArray (IMXUint8   a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MInt16   where { imxConstr = IMXInt16   ; imxArray (IMXInt16   a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MUint16  where { imxConstr = IMXUint16  ; imxArray (IMXUint16  a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MInt32   where { imxConstr = IMXInt32   ; imxArray (IMXInt32   a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MUint32  where { imxConstr = IMXUint32  ; imxArray (IMXUint32  a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MInt64   where { imxConstr = IMXInt64   ; imxArray (IMXInt64   a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem MUint64  where { imxConstr = IMXUint64  ; imxArray (IMXUint64  a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem (MComplex MDouble) where { imxConstr = IMXComplexDouble ; imxArray (IMXComplexDouble a) = Just a ; imxArray _ = Nothing }
instance IMXArrayElem (MComplex MSingle) where { imxConstr = IMXComplexSingle ; imxArray (IMXComplexSingle a) = Just a ; imxArray _ = Nothing }

-- |Generic 'IMXData' list constructor.  Specified 'MSize' may contain at most one -1 value, which will be inferred from the length of the list.
listIMX :: IMXArrayElem a => MSize -> [a] -> IMXData
listIMX s = imxConstr . listIMXArray s
-- |Generic 'IMXData' scalar constructor
scalarIMX :: IMXArrayElem a => a -> IMXData
scalarIMX = imxConstr . scalarIMXArray
emptyIMX :: IMXArrayElem a => a -> IMXData
emptyIMX a = listIMX [0] ([] `asTypeOf` [a])

-- |Generic 'IMXData' array size accessor
imxSize :: IMXData -> MSize
imxSize IMXNull = [0]
imxSize (IMXCell    a) = mRangeSize (bounds a)
imxSize (IMXLogical a) = mRangeSize (bounds a)
imxSize (IMXChar    a) = mRangeSize (bounds a)
imxSize (IMXDouble  a) = mRangeSize (bounds a)
imxSize (IMXSingle  a) = mRangeSize (bounds a)
imxSize (IMXInt8    a) = mRangeSize (bounds a)
imxSize (IMXUint8   a) = mRangeSize (bounds a)
imxSize (IMXInt16   a) = mRangeSize (bounds a)
imxSize (IMXUint16  a) = mRangeSize (bounds a)
imxSize (IMXInt32   a) = mRangeSize (bounds a)
imxSize (IMXUint32  a) = mRangeSize (bounds a)
imxSize (IMXInt64   a) = mRangeSize (bounds a)
imxSize (IMXUint64  a) = mRangeSize (bounds a)
imxSize (IMXComplexDouble a) = mRangeSize (bounds a)
imxSize (IMXComplexSingle a) = mRangeSize (bounds a)
imxSize (IMXStruct _ a) = mRangeSize (r0,r1) where ((r0,_),(r1,_)) = bounds a
imxSize (IMXObject _ d) = imxSize d

-- |Generic 'IMXData' list accessor
imxList :: IMXArrayElem a => IMXData -> Maybe [a]
imxList = fmap elems . imxArray
-- |Generic 'IMXData' scalar accessor
imxScalar :: IMXArrayElem a => IMXData -> Maybe a
imxScalar a = case imxList a of 
  Just [x] -> Just x
  _ -> Nothing

-- |Create a sized struct array from a sequential list of consecutive field values (@[i0f0,i0f1,...,i0fM,i1f0,i1f1,....,iNfM]@).
listIMXStruct :: [String] -> MSize -> [IMXData] -> IMXData
listIMXStruct f s l = IMXStruct f $ listArray r l where
  n = length f
  r = ((r0,0),(r1,pred n))
  (r0,r1) = mSizeRange $ tail $ fixMSize (n:s) l

-- |Access a struct as list of fields and list of consecutive field values
imxStructList :: IMXData -> Maybe ([String], [IMXData])
imxStructList (IMXStruct f v) = Just (f, elems v)
imxStructList _ = Nothing

-- |Create an immutable representation from an 'MXArray'
imxData :: MXArray a -> IO IMXData
imxData a = do
  t <- mxArrayClass a
  c <- if t /= MXClassNull then mxArrayIsComplex a else return undefined
  imxc t c where
  
  imxc :: MXClass -> Bool -> IO IMXData
  imxc MXClassNull _		= return IMXNull
  imxc MXClassCell False	= IMXCell	=.< imxa (imxData . mCell)
  imxc MXClassStruct False	= do
    s <- mxArraySize a'
    fv <- mxArrayGetAll a'
    f <- if null fv then mStructFields a' else return (map fst (mStruct (head fv)))
    listIMXStruct f s =.< mapM imxData (concatMap (map snd . mStruct) fv)
  imxc MXClassLogical False	= IMXLogical	=.< imxa return
  imxc MXClassChar False	= IMXChar	=.< imxa return
  imxc MXClassDouble False	= IMXDouble	=.< imxa return
  imxc MXClassSingle False	= IMXSingle	=.< imxa return
  imxc MXClassInt8 False	= IMXInt8	=.< imxa return
  imxc MXClassUint8 False	= IMXUint8	=.< imxa return
  imxc MXClassInt16 False	= IMXInt16	=.< imxa return
  imxc MXClassUint16 False	= IMXUint16	=.< imxa return
  imxc MXClassInt32 False	= IMXInt32	=.< imxa return
  imxc MXClassUint32 False	= IMXUint32	=.< imxa return
  imxc MXClassInt64 False	= IMXInt64	=.< imxa return
  imxc MXClassUint64 False	= IMXUint64	=.< imxa return
  imxc MXClassDouble True	= IMXComplexDouble	=.< imxa return
  imxc MXClassSingle True	= IMXComplexSingle	=.< imxa return
  imxc MXClassObject False	= do
    Just c <- mObjectGetClass a'
    IMXObject c =.< imxc MXClassStruct False
  imxc t c = fail ("imxData: unhandled mxArray type " ++ show t ++ if c then "(complex)" else "")

  imxa :: MXArrayComponent a => (a -> IO b) -> IO (IMXArray b)
  imxa f = do
    s <- mxArraySize a'
    listIMXArray s =.< mapM f =<< mxArrayGetAll a'

  a' = unsafeCastMXArray a

-- |Create a new 'MXArray' from a functional representation.
iMXData :: IMXData -> IO MAnyArray
iMXData = imxd where
  imxd :: IMXData -> IO MAnyArray
  imxd IMXNull = return $ anyMXArray mNullArray
  imxd (IMXCell a) = imxa a (MCell .=< iMXData)
  imxd (IMXStruct f a) = do
    let ((r0,_),(r1,_)) = bounds a
    m <- createStruct (mRangeSize (r0,r1)) f
    zipWithM (\i -> mStructSetFields m (mOffset i) <=< mapM iMXData) [0..] (segment (length f) (elems a))
    return $ anyMXArray m
  imxd (IMXLogical a)	= imxa a return
  imxd (IMXChar a)	= imxa a return
  imxd (IMXDouble a)	= imxa a return
  imxd (IMXSingle a)	= imxa a return
  imxd (IMXInt8 a)	= imxa a return
  imxd (IMXUint8 a)	= imxa a return
  imxd (IMXInt16 a)	= imxa a return
  imxd (IMXUint16 a)	= imxa a return
  imxd (IMXInt32 a)	= imxa a return
  imxd (IMXUint32 a)	= imxa a return
  imxd (IMXInt64 a)	= imxa a return
  imxd (IMXUint64 a)  	= imxa a return
  imxd (IMXComplexDouble a) = imxa a return
  imxd (IMXComplexSingle a) = imxa a return
  imxd (IMXObject c a) = do
    m <- imxd a
    mObjectSetClass (unsafeCastMXArray m) c
    return m

  imxa :: MXArrayComponent b => IMXArray a -> (a -> IO b) -> IO MAnyArray
  imxa a f = do
    m <- createMXArray (mRangeSize (bounds a))
    mxArraySetAll m =<< mapM f (elems a)
    return $ anyMXArray m

withIMXData :: With IMXData MAnyArray (IO a)
withIMXData d f = do
  a <- iMXData d
  r <- f a
  freeMXArray a
  return r

withIMXDataList :: With [IMXData] [MAnyArray] (IO a)
withIMXDataList = mapWith withIMXData

takeIMXData :: MXArray a -> IO IMXData
takeIMXData a = do
  d <- imxData a
  freeMXArray a
  return d

imxFun :: MFun -> IMXFun
imxFun fun a no =
  mapWith withIMXData a $ \m ->
  fun m no >>= mapM takeIMXData

iMXFun :: IMXFun -> MFun
iMXFun fun a no = do
  ia <- mapM imxData a
  fun ia no >>= mapM iMXData


showsApp :: String -> ShowS -> ShowS
showsApp f a s = f ++ '(' : a (')' : s)

{-
showsReshape :: (MIndex,MIndex) -> ShowS -> ShowS
showsReshape (MSubs [],MSubs []) a s = a s
showsReshape (MSubs [],MSubs []) a s = a s
showsReshape r a s = "reshape(" ++ a (',' : shows (realMSize $ mRangeSize r) (')' : s))

showsIMXArrayDelimWith :: (Char,Char,Char) -> (a -> ShowS) -> IMXArray a -> ShowS
showsIMXArrayDelimWith d f a = showsReshape (bounds a) $ showsMListWith d f (elems a)
showsIMXArrayWith :: (a -> ShowS) -> IMXArray a -> ShowS
showsIMXArrayWith = showsIMXArrayDelimWith ('[',',',']')
showsIMXArray :: Show a => IMXArray a -> ShowS
showsIMXArray = showsIMXArrayWith shows
-}

showsMList :: (Char,Char,Char) -> (a -> ShowS) -> [a] -> ShowS
showsMList (l,_,r) _ [] s = l : r : s
showsMList (l,d,r) f (x:xs) s = l : f x (shml xs) where
  shml [] = r : s
  shml (x:xs) = d : f x (shml xs)

showsReshape :: MSize -> (Char -> ShowS) -> ShowS
showsReshape [] f s = f ' ' s
showsReshape [_] f s = f ';' s
showsReshape [1,_] f s = f ',' s
showsReshape z f s = "reshape(" ++ f ' ' (',' : showsMList ('[',' ',']') shows (realMSize z) (')' : s))

showsIMXGenArray :: (Char,Char) -> (a -> ShowS) -> IMXArray a -> ShowS
showsIMXGenArray (l,r) f a = showsReshape (mRangeSize $ bounds a) $ \d -> showsMList (l,d,r) f (elems a)
showsIMXArrayWith :: (a -> ShowS) -> IMXArray a -> ShowS
showsIMXArrayWith = showsIMXGenArray ('[',']')
showsIMXArray :: Show a => IMXArray a -> ShowS
showsIMXArray = showsIMXArrayWith shows

showsComplex :: (Show a, RealFloat a) => Complex a -> ShowS
showsComplex (x:+y) s = "complex(" ++ shows x (',' : shows y (')' : s))

showsMString :: String -> ShowS
showsMString s = showChar '\'' . showString s . showChar '\''

showsIMX :: IMXData -> ShowS
showsIMX IMXNull = showString "[]"
showsIMX (IMXCell a) = showsIMXGenArray ('{','}') shows a
showsIMX (IMXStruct f a) = showsReshape (mRangeSize (r0,r1)) $ \d ->
  showString "struct" . showsMList ('(',',',')') (shf d) (zip f v) where
  shf d (f,v) s = showsMString f $ ',' : showsMList ('{',d,'}') showsIMX v s
  v = transpose $ segment (length f) $ elems a
  ((r0,_),(r1,_)) = bounds a
showsIMX (IMXLogical a) = showsApp "logical"  $ showsIMXArray a
showsIMX (IMXChar a)	= showsApp "char"     $ showsIMXArray a
showsIMX (IMXDouble a)	=                       showsIMXArray a
showsIMX (IMXSingle a)	= showsApp "single"   $ showsIMXArray a
showsIMX (IMXInt8 a)	= showsApp "int8"     $ showsIMXArray a
showsIMX (IMXUint8 a)	= showsApp "uint8"    $ showsIMXArray a
showsIMX (IMXInt16 a)	= showsApp "int16"    $ showsIMXArray a
showsIMX (IMXUint16 a)	= showsApp "uint16"   $ showsIMXArray a
showsIMX (IMXInt32 a)	= showsApp "int32"    $ showsIMXArray a
showsIMX (IMXUint32 a)	= showsApp "uint32"   $ showsIMXArray a
showsIMX (IMXInt64 a)	= showsApp "int64"    $ showsIMXArray a
showsIMX (IMXUint64 a)	= showsApp "uint64"   $ showsIMXArray a
showsIMX (IMXComplexDouble a)	= showsIMXArrayWith showsComplex a
showsIMX (IMXComplexSingle a)	= showsApp "single" $ showsIMXArrayWith showsComplex a
showsIMX (IMXObject c a) = showsApp "class" $ showsIMX a . showChar ',' . showsMString c

instance Show IMXData where
  showsPrec _ = showsIMX
