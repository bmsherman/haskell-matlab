{-# LANGUAGE LambdaCase         #-}

module Foreign.Matlab.ZIOTypes (
  mxeeZ
, mxreE
, mxreZ
, mxToMaybeE, mxToMaybeZ
, module Foreign.Matlab.Exceptions
) where

import qualified Control.Exception as Ex
import           Foreign.Matlab.Exceptions
import           ZIO.Trans

mxeeZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxeeZ = mapZError (\e -> MXEngineError (Ex.toException e))

mxreE :: EIO SomeNonPseudoException a -> EIO MatlabException a
mxreE = mapEError (\e -> MXRuntimeError (Ex.toException e))

mxreZ :: ZIO r SomeNonPseudoException a -> ZIO r MatlabException a
mxreZ = mapZError (\e -> MXRuntimeError (Ex.toException e))

mxToMaybeE :: EIO MatlabException a -> EIO MatlabException (Maybe a)
mxToMaybeE eio = catchError (pure <$> eio) (\case
  MXNothing -> pure Nothing
  _ -> (pure <$> eio)
  )

mxToMaybeZ :: ZIO r MatlabException a -> ZIO r MatlabException (Maybe a)
mxToMaybeZ zio = do
  env <- ask
  (ezlift . mxToMaybeE . (flip runReaderT env)  . _unZIO) zio
