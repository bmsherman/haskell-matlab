{-|
 ZIO-based wrappers to common (and some uncommon) MATLAB functions using the MATLAB Engine.

 Note that you cannot use "Foreign.Matlab.Engine" and "Foreign.Matlab.Runtime" in the same program.
 This seems to be a Matlab limitation.
 -}

{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Matlab.ZIOEngine.Wrappers (
  addpath
, clearVar
, disp
, getArrayFromByteStream
, getByteStreamFromArray
, cd, pwd
, MXMap(..), mmapKeys, mmapToHmap
, EW.MEither(..), isMLeft, isMRight
, EW.VarArgIn, EW.mxVarArgs
) where

import qualified Data.Map.Strict as DM
import           Foreign.Matlab
import qualified Foreign.Matlab.Array as A
import qualified Foreign.Matlab.Engine.Wrappers as EW
import qualified Foreign.Matlab.ZIOArray as ZA
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans


-- | We require an absolute path in this case
addpath :: HasEngine r => Path Abs Dir -> ZIO r MatlabException ()
addpath p = engineEvalProc "addpath" [EvalString $ toFilePath p]

-- | TODO: add a test for this, likely not working as expected.
-- | Clears a variable from the engine's workspace
clearVar :: HasEngine r => String -> ZIO r MatlabException ()
clearVar var = engineEvalProc "clear" [EvalString var]

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
  (headZ "getArrayFromByteStream returned 0 values") =<<
    engineEvalFun "getArrayFromByteStream" [EvalArray matBsArr] 1
  -- mapZError MXLogicalError $ liftEither $
  --   maybeToRight "getArrayFromByteStream returned 0 values" $ listToMaybe evalFunRes

isMLeft :: EW.MEither -> ZIO r MatlabException Bool
isMLeft me = do
  sFields <- ZA.mStructFields $ EW.unMXEither me
  pure $ "left" `elem` sFields

isMRight :: EW.MEither -> ZIO r MatlabException Bool
isMRight me = do
  sFields <- ZA.mStructFields $ EW.unMXEither me
  pure $ "right" `elem` sFields

--- --- --- Functions currently in ZIO Wrappers only follow --- --- ---

cd :: HasEngine r => (Path Abs Dir) -> ZIO r MatlabException ()
cd dir = engineEvalProc "cd" [EvalString $ fromAbsDir dir]

pwd :: HasEngine r => ZIO r MatlabException (Path Abs Dir)
pwd = do
  pwdDirAnyArr <- headZ "pwd returned nothing" =<< engineEvalFun "pwd" [] 1
  pwdDirCArr <- ZA.castMXArray pwdDirAnyArr
  dir <- ZA.mxArrayGetAll pwdDirCArr
  mxleZ . zlift $ parseAbsDir dir

disp :: HasEngine r => MXArray a -> ZIO r MatlabException ()
disp a = engineEvalProc "disp" [EvalArray $ ZA.anyMXArray a]

newtype MXMap = MXMap { _mxMap :: MAnyArray }

mmapToHmap :: forall r a. (HasEngine r, ZA.MXArrayComponent a)
  => MXMap -> ZIO r MatlabException (DM.Map String (MXArray a))
mmapToHmap mmap = do
  keys <- mmapKeys mmap
  valuesCell :: MXArray MCell <- engineEvalFun "values" [
      EvalArray $ A.anyMXArray $ _mxMap mmap
    , EvalArray $ A.anyMXArray $ keys] 1
    >>= headZ "No results from engineEvalFun:values" >>= ZA.castMXArray
  keyList <- ZA.mxCellGetAllListsOfType keys
  valueList <- ZA.mxCellGetArraysOfType valuesCell
  pure $ DM.fromList $ zip keyList valueList


-- | Low-level interface to get a container.Map's keys
mmapKeys :: HasEngine r => MXMap -> ZIO r MatlabException (MXArray MCell)
mmapKeys mmap = engineEvalFun "keys" [EvalArray $ _mxMap mmap] 1
  >>= headZ "No results from mmapKeys" >>= ZA.castMXArray