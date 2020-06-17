{-# LANGUAGE DeriveDataTypeable #-}

module Foreign.Matlab.Exceptions where

import qualified Control.Exception as Ex
import           Data.Typeable (Typeable)

data MatlabException =
    MXLogicalError String -- ^ 
  | MXRuntimeError Ex.SomeException
  | MXEngineError Ex.SomeException
  | MXNothing -- ^ To avoid wrapping Maybes by default.
  | MXLibError -- ^ Error creatined in a downstream library
  | MXAppError -- ^ Error creatined in a downstream application
  deriving (Show, Typeable)

instance Ex.Exception MatlabException

