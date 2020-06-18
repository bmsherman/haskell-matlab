{-# LANGUAGE LambdaCase         #-}

module Foreign.Matlab.ZIOTypes (
  mxreE
, mxeeZ, mxreZ
, mxaeE, mxleE
, mxaeZ, mxleZ
, mxToMaybeE, mxToMaybeZ
, module Foreign.Matlab.Exceptions
) where

import qualified Control.Exception as Ex
import           Foreign.Matlab.Exceptions
import           ZIO.Trans

mxreE :: EIO SomeNonPseudoException a -> EIO MatlabException a
mxreE = mapEError (\e -> MXRuntimeError (Ex.toException e))

mxeeZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxeeZ = mapZError (\e -> MXEngineError (Ex.toException e))

mxreZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxreZ = mapZError (\e -> MXRuntimeError (Ex.toException e))

mxaeE :: EIO SomeNonPseudoException a -> EIO MatlabException a
mxaeE = mapEError (\e -> MXAppError (Ex.toException e))

mxleE :: EIO SomeNonPseudoException a -> EIO MatlabException a
mxleE = mapEError (\e -> MXLibError (Ex.toException e))

mxaeZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxaeZ = mapZError (\e -> MXAppError (Ex.toException e))

mxleZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxleZ = mapZError (\e -> MXLibError (Ex.toException e))

mxToMaybeE :: EIO MatlabException a -> EIO MatlabException (Maybe a)
mxToMaybeE eio = catchError (pure <$> eio) (\case
  MXNothing -> pure Nothing
  _ -> (pure <$> eio)
  )

mxToMaybeZ :: ZIO r MatlabException a -> ZIO r MatlabException (Maybe a)
mxToMaybeZ zio = do
  env <- ask
  (ezlift . mxToMaybeE . (flip runReaderT env)  . _unZIO) zio
