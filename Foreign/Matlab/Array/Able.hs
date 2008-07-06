{-|
  Direct conversion between Haskell data structures and 'IMXData'.

  In general, scalars convert to the obvious, and lists to row vectors.
-}
module Foreign.Matlab.Array.Able (
    Matlabable,
    toMatlab, fromMatlab,
    withMatlabArray, fromMatlabArray
  ) where

import Foreign.Matlab.Util
import Foreign.Matlab.Types
import Foreign.Matlab.Array.IMX

class Matlabable a where
  toMatlab :: a -> IMXData
  fromMatlab :: IMXData -> Maybe a

--instance (MScalar a, IMXArrayElem a) => Matlabable a where { toMatlab = scalarIMX ; fromMatlab = imxScalar }
instance Matlabable Bool   where { toMatlab = scalarIMX ; fromMatlab = imxScalar }
instance Matlabable Char   where { toMatlab = scalarIMX ; fromMatlab = imxScalar }
instance Matlabable Float  where { toMatlab = scalarIMX ; fromMatlab = imxScalar }
instance Matlabable Double where { toMatlab = scalarIMX ; fromMatlab = imxScalar }

instance Matlabable Int where 
  toMatlab = scalarIMX . (ii :: Int -> MInt32)
  fromMatlab = fmap (ii :: MInt32 -> Int) . imxScalar

instance Matlabable String where
  toMatlab = listIMX [1,-1]
  fromMatlab = imxList 

instance Matlabable [Double] where
  toMatlab = listIMX [1,-1]
  fromMatlab = imxList 

instance Matlabable [Int] where
  toMatlab = listIMX [1,-1] . map (ii :: Int -> MInt32)
  fromMatlab = fmap (map (ii :: MInt32 -> Int)) . imxList

instance Matlabable () where
  toMatlab () = IMXNull
  fromMatlab IMXNull = Just ()
  fromMatlab _ = Nothing

-- |Generate a temporary 'MXArray'
withMatlabArray :: Matlabable a => a -> (MAnyArray -> IO a) -> IO a
withMatlabArray = withIMXData . toMatlab

-- |Convert directly from 'MXArray' (without freeing the original array)
fromMatlabArray :: Matlabable a => MAnyArray -> IO (Maybe a)
fromMatlabArray = fromMatlab .=< imxData
