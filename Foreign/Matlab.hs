{-# OPTIONS_GHC -fno-implicit-prelude #-}

{-|
  Bundles Matlab data structure and general-purpose routines.
-}

module Foreign.Matlab
  ( module Foreign.Matlab.Types
  , module Foreign.Matlab.Array
  , module Foreign.Matlab.Array.Auto
  --, module Foreign.Matlab.Array.MArray
  , module Foreign.Matlab.Array.IMX
  , module Foreign.Matlab.Array.Able
  , module Foreign.Matlab.MAT
  ) where

import Foreign.Matlab.Types
import Foreign.Matlab.Array
import Foreign.Matlab.Array.Auto
--import Foreign.Matlab.Array.MArray
import Foreign.Matlab.Array.IMX
import Foreign.Matlab.Array.Able
import Foreign.Matlab.MAT
