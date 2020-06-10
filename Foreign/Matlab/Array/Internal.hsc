{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

module Foreign.Matlab.Array.Internal where

-- import           Control.Monad
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
-- import           Data.Complex
import qualified Data.Map.Strict as DM
-- import           Data.Maybe (catMaybes)
import           Foreign.Matlab.Util
import           Foreign.Matlab.Internal

foreign import ccall unsafe mxGetData :: MXArrayPtr -> IO (Ptr a)
