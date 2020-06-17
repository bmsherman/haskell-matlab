{-|
 ZIO-based wrappers to common (and some uncommon) MATLAB functions using the MATLAB Engine.

 Note that you cannot use "Foreign.Matlab.Engine" and "Foreign.Matlab.Runtime" in the same program.
 This seems to be a Matlab limitation.
 -}

module Foreign.Matlab.ZIOEngine.Wrappers (
  addpath
, clearVar
, getArrayFromByteStream
, getByteStreamFromArray
, EW.MEither(..), isMLeft, isMRight
, EW.VarArgIn, EW.mxVarArgs
) where

import           Data.Either.Combinators (maybeToRight)
import           Data.Maybe (listToMaybe)
import           Foreign.Matlab
import qualified Foreign.Matlab.Engine.Wrappers as EW
import qualified Foreign.Matlab.ZIOArray as ZA
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans


-- | We require an absolute path in this case
addpath :: HasEngine r => Path Abs Dir -> ZIO r MatlabException ()
addpath p = engineEvalProc "addpath" [EvalStr $ toFilePath p]

-- | TODO: add a test for this, likely not working as expected.
-- | Clears a variable from the engine's workspace
clearVar :: HasEngine r => String -> ZIO r MatlabException ()
clearVar var = engineEvalProc "clear" [EvalStr var]

-- | Wraps an undocumented function to serialize a MATLAB object.
getByteStreamFromArray :: HasEngine r => MAnyArray -> ZIO r MatlabException [MUint8]
getByteStreamFromArray mObj = do
  env <- ask
  let eng = getEngine env
  bsEi <- mxeeZ . zlift $ EW.getByteStreamFromArray eng mObj
  mapZError MXLogicalError $ liftEither bsEi

-- | Wraps an undocumented function to deserialize a MATLAB object.
getArrayFromByteStream :: HasEngine r => [MUint8] -> ZIO r MatlabException MAnyArray
getArrayFromByteStream bytes = do
  matBsArr <- ZA.createMXArray [length bytes]
  ZA.mxArraySetAll matBsArr bytes
  evalFunRes <- engineEvalFun "getArrayFromByteStream" [EvalArray matBsArr] 1
  mapZError MXLogicalError $ liftEither $
    maybeToRight "getArrayFromByteStream returned 0 values" $ listToMaybe evalFunRes

isMLeft :: EW.MEither -> ZIO r MatlabException Bool
isMLeft me = do
  sFields <- ZA.mStructFields $ EW.unMXEither me
  pure $ "left" `elem` sFields

isMRight :: EW.MEither -> ZIO r MatlabException Bool
isMRight me = do
  sFields <- ZA.mStructFields $ EW.unMXEither me
  pure $ "right" `elem` sFields
