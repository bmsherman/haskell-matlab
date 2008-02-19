{-|
  Safe MArray interface to "Matlab.Array".

  An instance of 'Data.Array.MArray.MArray' for 'MXArray'.  Although array access can be done through this instance, arrays themselves must be created using 'mxMArray' along with one of the "Matlab.Array" create functions.
-}
module Foreign.Matlab.Array.MArray (
    MMXArray, mxMArray, unMmxArray
  ) where

import qualified Data.Ix as Ix
import qualified Data.Array.Base as DA
import qualified Unsafe.Coerce
import Foreign.Matlab.Util
import Foreign.Matlab.Types
import Foreign.Matlab.Array

-- |A wrapper for 'MXArray' to allow for a 'Data.Array.MArray.MArray' instance.  All instances of this type will have 'MIndex' as @i@
newtype MMXArray i e = MMXArray { mmxArray :: MXArray e }

-- |Get an 'MArray' instance for an 'MXArray'.  The resulting object is just a different interface to the same underlying array.
mxMArray :: MXArrayComponent a => MXArray a -> MMXArray MIndex a
mxMArray = MMXArray
-- |Get a 'MXArray' back from its 'MArray' instance
unMmxArray :: MXArrayComponent a => MMXArray MIndex a -> MXArray a
unMmxArray = mmxArray

mix :: Ix.Ix a => MIndex -> a
mix x = Unsafe.Coerce.unsafeCoerce x
mir :: Ix.Ix a => (MIndex,MIndex) -> (a,a)
mir (x,y) = (mix x,mix y)

instance MXArrayComponent e => DA.MArray MMXArray e IO where
  getBounds = mir . mSizeRange .=< mxArraySize . mmxArray
  getNumElements = mxArrayLength . mmxArray
  newArray_ _ = fail "MMXArray.newArray_: use MXArrayI . createMXArray"
  unsafeRead = mxArrayGetOffset . mmxArray
  unsafeWrite = mxArraySetOffset . mmxArray

