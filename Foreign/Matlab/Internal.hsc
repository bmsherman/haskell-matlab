module Foreign.Matlab.Internal (
    CBool, boolC, cBool,
    MIO,
    MType(..),
    MXClassID, MXClass(..),
    MXChar, MChar,
    MXLogical, MLogical,
    MXDouble, MDouble,
    MXSingle, MSingle,
    MXInt8, MInt8,
    MXInt16, MInt16,
    MXInt32, MInt32,
    MXInt64, MInt64,
    MXUint8, MUint8,
    MXUint16, MUint16,
    MXUint32, MUint32,
    MXUint64, MUint64,
    MXArrayType,
    MXArrayPtr, MXArray(..),
    mkMXArray, withMXArray,
    unsafeCastMXArray,
    MAny, MAnyArray,
    MNull, mNullArray, isMNull,
    MCell(..),
    MStruct(..),
    MXFun, MFun,
    MWSize, MWIndex, MWSignedIndex
  ) where

import Foreign
import Foreign.C.Types
import qualified Data.Char
import Data.Typeable
import Foreign.Matlab.Util

#include <matrix.h>

type MIO a = IO a

type CBool = #type bool

boolC :: CBool -> Bool
boolC = (0 /=)

cBool :: Bool -> CBool
cBool = ii . fromEnum

type MXClassID = #type mxClassID
data MXClass = 
    MXClassNull
  | MXClassCell 
  | MXClassStruct
  | MXClassLogical
  | MXClassChar
  | MXClassDouble
  | MXClassSingle
  | MXClassInt8
  | MXClassUint8
  | MXClassInt16
  | MXClassUint16
  | MXClassInt32
  | MXClassUint32
  | MXClassInt64
  | MXClassUint64
  | MXClassFun
  | MXClassObject
  deriving (Eq, Show)

-- |A type equivalence between a Matlab and Haskell type
class MType mx a | a -> mx where
  hs2mx :: a -> mx
  mx2hs :: mx -> a
  mxClassOf :: a -> MXClass

instance MType MXClassID MXClass where
  mx2hs (#const mxVOID_CLASS) = MXClassNull
  mx2hs (#const mxCELL_CLASS)    = MXClassCell
  mx2hs (#const mxSTRUCT_CLASS)  = MXClassStruct
  mx2hs (#const mxLOGICAL_CLASS) = MXClassLogical
  mx2hs (#const mxCHAR_CLASS)    = MXClassChar
  mx2hs (#const mxDOUBLE_CLASS)  = MXClassDouble
  mx2hs (#const mxSINGLE_CLASS)  = MXClassSingle
  mx2hs (#const mxINT8_CLASS)    = MXClassInt8
  mx2hs (#const mxUINT8_CLASS)   = MXClassUint8
  mx2hs (#const mxINT16_CLASS)   = MXClassInt16
  mx2hs (#const mxUINT16_CLASS)  = MXClassUint16
  mx2hs (#const mxINT32_CLASS)   = MXClassInt32
  mx2hs (#const mxUINT32_CLASS)  = MXClassUint32
  mx2hs (#const mxINT64_CLASS)   = MXClassInt64
  mx2hs (#const mxUINT64_CLASS)  = MXClassUint64
  mx2hs (#const mxFUNCTION_CLASS)= MXClassFun
  mx2hs (#const mxOBJECT_CLASS)	 = MXClassObject
  mx2hs c = error ("MXClass: unknown mxClassID " ++ show c)
  hs2mx MXClassNull     = #const mxVOID_CLASS
  hs2mx MXClassCell     = #const mxCELL_CLASS
  hs2mx MXClassStruct   = #const mxSTRUCT_CLASS
  hs2mx MXClassLogical  = #const mxLOGICAL_CLASS
  hs2mx MXClassChar     = #const mxCHAR_CLASS
  hs2mx MXClassDouble   = #const mxDOUBLE_CLASS
  hs2mx MXClassSingle   = #const mxSINGLE_CLASS
  hs2mx MXClassInt8     = #const mxINT8_CLASS
  hs2mx MXClassUint8    = #const mxUINT8_CLASS
  hs2mx MXClassInt16    = #const mxINT16_CLASS
  hs2mx MXClassUint16   = #const mxUINT16_CLASS
  hs2mx MXClassInt32    = #const mxINT32_CLASS
  hs2mx MXClassUint32   = #const mxUINT32_CLASS
  hs2mx MXClassInt64    = #const mxINT64_CLASS
  hs2mx MXClassUint64   = #const mxUINT64_CLASS
  hs2mx MXClassFun      = #const mxFUNCTION_CLASS
  hs2mx MXClassObject   = #const mxOBJECT_CLASS	 
  mxClassOf _ = error "mxClassOf: no class for MXClassID"

type MXChar = #type mxChar
type MChar = Char
instance MType MXChar MChar where
  hs2mx = ii . Data.Char.ord
  mx2hs = Data.Char.chr . ii
  mxClassOf _ = MXClassChar

type MXLogical = #type mxLogical
type MLogical = Bool
instance MType MXLogical MLogical where
  hs2mx = cBool
  mx2hs = boolC
  mxClassOf _ = MXClassLogical

type MXDouble = Double
type MDouble = Double
instance MType MXDouble MDouble where
  hs2mx = id
  mx2hs = id
  mxClassOf _ = MXClassDouble
type MXSingle = Float
type MSingle = Float
instance MType MXSingle MSingle where
  hs2mx = id
  mx2hs = id
  mxClassOf _ = MXClassSingle

#let inttype u, n = "\
type MX%2$s%1$u = %3$s%1$u\n\
type M%2$s%1$u = %3$s%1$u\n\
instance MType MX%2$s%1$u M%2$s%1$u where { hs2mx = id ; mx2hs = id ; mxClassOf _ = MXClass%2$s%1$u }\
", n, u ? "Uint" : "Int", u ? "Word" : "Int"
--"

#inttype 0, 8
#inttype 0, 16
#inttype 0, 32
#inttype 0, 64
#inttype 1, 8
#inttype 1, 16
#inttype 1, 32
#inttype 1, 64

data MXArrayType
type MXArrayPtr = Ptr MXArrayType

-- |The general Matlab Array type, used for most all Matlab data
newtype MXArray a = MXArray { mxArray :: MXArrayPtr }

mkMXArray :: MXArrayPtr -> IO (MXArray a)
mkMXArray = return . MXArray

withMXArray :: With (MXArray x) MXArrayPtr a
withMXArray (MXArray a) f = f a

unsafeCastMXArray :: MXArray a -> MXArray b
unsafeCastMXArray = MXArray . castPtr . mxArray

-- |Determine whether the given array is NULL
isMNull :: MXArray a -> Bool
isMNull (MXArray a) = nullPtr == a

-- |Tag for a generic array
data MAny
-- |A generic, untyped (void) array, which must be cast (using 'Foreign.Matlab.Array.castMXArray')
type MAnyArray = MXArray MAny

-- |Tag for a NULL array
data MNull deriving (Typeable)
instance MType MNull MNull where mxClassOf _ = MXClassNull

mNullArray :: MXArray MNull
mNullArray = MXArray nullPtr

-- |A wrapper for a member of a cell array, which itself simply any other array
newtype MCell = MCell { mCell :: MAnyArray } deriving (Typeable)
instance MType MCell MCell where mxClassOf _ = MXClassCell

-- |A single struct in an array, represented by an (ordered) list of key-value pairs
newtype MStruct = MStruct { mStruct :: [(String,MAnyArray)] } deriving (Typeable)
instance MType MStruct MStruct where mxClassOf _ = MXClassStruct

type MXFun = CInt -> Ptr MXArrayPtr -> CInt -> Ptr MXArrayPtr -> IO ()
-- |A Matlab function
type MFun = 
  [MAnyArray] -- ^ RHS input arguments
  -> Int -- ^ LHS output argument count
  -> IO [MAnyArray] -- ^ LHS output arguments
instance MType MXFun MFun where
  hs2mx fun outn outp argn argp = do
    arg <- map MXArray =.< peekArray (ii argn) argp
    out <- fun arg (ii outn)
    pokeArray outp $ map mxArray out
  mx2hs fun arg no =
    withArrayLen (map mxArray arg) $ \argn argp ->
    allocaArray no $ \outp -> do
    fun (ii no) outp (ii argn) argp
    map MXArray =.< peekArray no outp
  mxClassOf _ = MXClassFun

type MWSize = #type mwSize
type MWIndex = #type mwIndex
type MWSignedIndex = #type mwSignedIndex
