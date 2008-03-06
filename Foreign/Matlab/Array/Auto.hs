{-|
  'MXArray' 'ForeignPtr' wrapper.

  A 'MXArray' that is automatically freed.  These arrays must never be put inside other arrays or used as function results.
-}
module Foreign.Matlab.Array.Auto (
    MXAuto,
    mxAuto,
    withMXAuto,
    MAnyAuto
  ) where

import Foreign
import Foreign.Matlab.Util
import Foreign.Matlab.Internal
import Foreign.Matlab.Types

-- |A 'MXArray' that is automatically freed with 'Foreign.Matlab.Array.freeMXArray'
newtype MXAuto a = MXAuto (ForeignPtr MXArrayType)

foreign import ccall unsafe "&mxDestroyArray" mxDestroyArray_ptr :: FunPtr (MXArrayPtr -> IO ())

-- |Turn an 'MXArray' into an 'MXAuto'.  The original 'MXArray' should not be used after this operation.
mxAuto :: MXArray a -> MIO (MXAuto a)
mxAuto (MXArray a)
  | a == nullPtr = MXAuto =.< newForeignPtr_ a
  | otherwise = MXAuto =.< newForeignPtr mxDestroyArray_ptr a

-- |Use a 'MXAuto'
withMXAuto :: With (MXAuto a) (MXArray a) (IO b)
withMXAuto (MXAuto a) f = withForeignPtr a (f . MXArray)

type MAnyAuto = MXAuto MAny
