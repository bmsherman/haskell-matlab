{-|
  ZIO Interface to a Matlab engine.
  This works by spawning a separate matlab process and interchanging data in MAT format.

  Note that you cannot use "Foreign.Matlab.Engine" and "Foreign.Matlab.Runtime" in the same program.
  This seems to be a Matlab limitation.
-}
module Foreign.Matlab.ZIOEngine (
    E.Engine,
    E.newEngine,
    engineEval,
    engineGetVar,
    engineSetVar,
    E.EngineEvalArg(..),
    engineEvalEngFun,
    engineEvalFun,
    engineEvalProc,
    E.HasEngine(..), E.SetEngine(..),
    E.qt
  ) where

import qualified Foreign.Matlab.Engine as E
import           Foreign.Matlab.Internal
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

-- |Execute matlab statement
engineEval :: E.HasEngine r => String -> ZIO r MatlabException ()
engineEval s = ask >>= \r -> mxeeZ . zlift $ E.engineEval (E.getEngine r) s

-- |Get a variable with the specified name from MATLAB's workspace
engineGetVar :: E.HasEngine r => String -> ZIO r MatlabException (MXArray a)
engineGetVar v = ask >>= \r -> mxeeZ . zlift $ E.engineGetVar (E.getEngine r) v

-- |Put a variable into MATLAB's workspace with the specified name
engineSetVar :: E.HasEngine r => String -> MXArray a -> ZIO r MatlabException ()
engineSetVar v x = ask >>= \r -> mxeeZ . zlift $ E.engineSetVar (E.getEngine r) v x

-- |Evaluate a function with the given arguments and number of results.
-- This automates 'engineSetVar' on arguments (using \"hseval_inN\"), 'engineEval', and 'engineGetVar' on results (using \"hseval_outN\").
engineEvalFun :: E.HasEngine r => String -> [E.EngineEvalArg a] -> Int -> ZIO r MatlabException [MAnyArray]
engineEvalFun fun args no = ask >>= \r -> mxeeZ . zlift $ E.engineEvalFun (E.getEngine r) fun args no

-- |Like `engineEvalFun` but returns wrapped variables in the Engine.
-- |Since variables are persistent, they have names based on UUIDs.
engineEvalEngFun :: E.HasEngine r => String -> [E.EngineEvalArg a] -> Int -> ZIO r MatlabException [E.MEngVar]
engineEvalEngFun fun args no = ask >>= \r -> mxeeZ . zlift $ E.engineEvalEngFun (E.getEngine r) fun args no

-- |Convenience function for calling functions that do not return values (i.e. "procedures").
engineEvalProc :: E.HasEngine r => String -> [E.EngineEvalArg a] -> ZIO r MatlabException ()
engineEvalProc fun args = ask >>= \r -> mxeeZ . zlift $ E.engineEvalProc (E.getEngine r) fun args

