{-|
 Wrappers to common MATLAB functions using the Matlab engine.

 Note that you cannot use "Foreign.Matlab.Engine" and "Foreign.Matlab.Runtime" in the same program.
 This seems to be a Matlab limitation.
 -}

module Foreign.Matlab.Engine.Wrappers (
         addpath
       ) where

import Foreign.Matlab.Engine
import Path

-- | We require an absolute path in this case
addpath :: Engine -> Path Abs Dir -> IO ()
addpath eng p = do
  engineEvalProc eng "addpath" [EvalStr $ toFilePath p]
  -- engineEval eng $ "addpath(" <> toFilePath p <> "');"
