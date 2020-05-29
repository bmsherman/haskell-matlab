{-|
 Wrappers to common (and some uncommon) MATLAB functions using the MATLAB Engine.

 Note that you cannot use "Foreign.Matlab.Engine" and "Foreign.Matlab.Runtime" in the same program.
 This seems to be a Matlab limitation.
 -}

module Foreign.Matlab.Engine.Wrappers (
  addpath
, clearVar
, getArrayFromByteStream
, getByteStreamFromArray
, MEither(..), isMLeft, isMRight
) where

import Foreign.Matlab
import Foreign.Matlab.Engine
import Path

-- | We require an absolute path in this case
addpath :: Engine -> Path Abs Dir -> IO ()
addpath eng p = engineEvalProc eng "addpath" [EvalStr $ toFilePath p]

--TODO: add a test for this
-- | Clears a variable from the engine's workspace
clearVar :: Engine -> String -> IO ()
clearVar eng var = engineEvalProc eng "clear" [EvalStr var]

-- | Wraps an undocumented function to serialize a MATLAB object.
getByteStreamFromArray :: Engine -> MAnyArray -> IO (Either String [MUint8])
getByteStreamFromArray eng mObj = do
  [byteStream] <- engineEvalFun eng "getByteStreamFromArray" [EvalArray mObj] 1
  mxArrMay <- castMXArray byteStream
  case mxArrMay of
    Just mxArr -> Right <$> mxArrayGetAll mxArr
    Nothing -> pure $ Left
      "getByteStreamFromArray: Couldn't convert MATLAB bytestream to [MUint8]"

-- | Wraps an undocumented function to deserialize a MATLAB object.
getArrayFromByteStream :: Engine -> [MUint8] -> IO MAnyArray
getArrayFromByteStream eng bytes = do
  matBsArr <- createMXArray [length bytes]
  mxArraySetAll matBsArr bytes
  [mObj] <- engineEvalFun eng "getArrayFromByteStream" [EvalArray matBsArr] 1
  pure mObj

newtype MEither = MEither {unMXEither :: MStructArray}

isMLeft :: MEither -> IO Bool
isMLeft me = do
  sFields <- mStructFields $ unMXEither me
  pure $ "left" `elem` sFields

isMRight :: MEither -> IO Bool
isMRight me = do
  sFields <- mStructFields $ unMXEither me
  pure $ "right" `elem` sFields
