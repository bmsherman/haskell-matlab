{-|
  Interface to a Matlab engine.
  This works by spawning a separate matlab process and interchanging data in MAT format.

  Note that you cannot use "Matlab.Engine" and "Matlab.Runtime" in the same program.
  This seems to be a Matlab limitation.
-}
module Foreign.Matlab.Engine (
    Engine,
    newEngine,
    engineEval,
    engineGetVar,
    engineSetVar,
    EngineEvalArg(..),
    engineEvalFun
  ) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.List
import Foreign.Matlab.Util
import Foreign.Matlab.Internal
import Foreign.Matlab.Types
import Foreign.Matlab.Config

#include <engine.h>

data EngineType
type EnginePtr = Ptr EngineType

-- |A Matlab engine instance
newtype Engine = Engine (ForeignPtr EngineType)

foreign import ccall unsafe engOpen :: CString -> IO EnginePtr
foreign import ccall unsafe "&" engClose :: FunPtr (EnginePtr -> IO ()) -- CInt

-- |Start Matlab server process.  It will automatically be closed down when no longer in use.
newEngine :: Maybe FilePath -> IO Engine
newEngine Nothing = newEngine (Just matlabBin)
newEngine (Just bin) = do
  eng <- withCString bin engOpen
  if eng == nullPtr
    then fail "engOpen"
    else Engine =.< newForeignPtr engClose eng

withEngine :: Engine -> (EnginePtr -> IO a) -> IO a
withEngine (Engine eng) = withForeignPtr eng

foreign import ccall unsafe engEvalString :: EnginePtr -> CString -> IO CInt
-- |Execute matlab statement
engineEval :: Engine -> String -> IO ()
engineEval eng s = do 
  r <- withEngine eng (withCString s . engEvalString)
  when (r /= 0) $ fail "engineEval"

foreign import ccall unsafe engGetVariable :: EnginePtr -> CString -> IO MXArrayPtr
-- |Get a variable with the specified name from MATLAB's workspace
engineGetVar :: Engine -> String -> IO (MXArray a)
engineGetVar eng v = withEngine eng (withCString v . engGetVariable) >>= mkMXArray

foreign import ccall unsafe engPutVariable :: EnginePtr -> CString -> MXArrayPtr -> IO CInt
-- |Put a variable into MATLAB's workspace with the specified name
engineSetVar :: Engine -> String -> MXArray a -> IO ()
engineSetVar eng v x = do
  r <- withEngine eng (\eng -> withCString v (withMXArray x . engPutVariable eng))
  when (r /= 0) $ fail "engineSetVar"

data EngineEvalArg a = EvalArray (MXArray a) | EvalVar String

-- |Evaluate a function with the given arguments and number of results.
-- This automates 'engineSetVar' on arguments (using \"hseval_inN\"), 'engineEval', and 'engineGetVar' on results (using \"hseval_outN\").
engineEvalFun :: Engine -> String -> [EngineEvalArg a] -> Int -> IO [MAnyArray]
engineEvalFun eng fun arg no = do
  arg <- zipWithM makearg arg [1 :: Int ..]
  let out = map makeout [1..no]
  let outs = if out == [] then "" else "[" ++ unwords out ++ "] = "
  engineEval eng (outs ++ fun ++ "(" ++ intercalate "," arg ++ ")")
  mapM (engineGetVar eng) out
  where
    makearg (EvalArray x) i = do
      let v = "hseval_in" ++ show i
      engineSetVar eng v x
      return v
    makearg (EvalVar v) _ = return v
    makeout i = "hseval_out" ++ show i
