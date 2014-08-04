{-|
  Generic runtime library interface.
  This uses a Matlab library built along with HSMatlab to provide runtime access to general Matlab functionality throuh "Foreign.Matlab.Runtime".
  Any builtin Matlab function can be called through this interface, but other M-file functions may not -- these need to be put into a library or used through "Foreign.Matlab.Engine" instead.
-}
module Foreign.Matlab.Runtime.Generic (
    MLGeneric,
    openMLGeneric,
    closeMLGeneric,
    mlGenericEval,
    mlGenericFun,
    mlGenericSetVar,
    mlGenericGetVar,
    mlGenericCapture
  ) where

import Foreign.C.String
--import System.FilePath
import Foreign.Matlab.Util
import Foreign.Matlab.Types
import Foreign.Matlab.Array
import Foreign.Matlab.Runtime
--import Paths_matlab

data MLGeneric = MLGeneric {
    mlgLibrary :: MLibrary,
    mlgFeval :: CString -> MFun
  }

-- |Create a generic interface to the Matlab runtime.  Only one is necessary in any given application.
openMLGeneric :: [String] -> IO MLGeneric
openMLGeneric opt = do
  -- libdir <- getLibDir
  ml <- openMLibrary ({- libdir </> -} "hsmatlab") opt
  feval <- mlGenericFeval ml
  return $ MLGeneric { mlgLibrary = ml, mlgFeval = feval }

-- |Close a MLGeneric interface no longer in use.
closeMLGeneric :: MLGeneric -> IO ()
closeMLGeneric MLGeneric{ mlgLibrary = ml } = closeMLibrary ml

-- |Call the named Matlab function using the specified interface.
mlGenericFun :: MLGeneric -> String -> MFun
mlGenericFun mlg f a n = do
  withCString f (\f -> mlgFeval mlg f (map anyMXArray a) n)

-- |Eval Matlab code.
mlGenericEval :: MLGeneric 
  -> String -- ^ The Matlab code string to evaluate
  -> Int -- ^ The number of arguments returned by the code
  -> IO [MAnyArray]
mlGenericEval mlg e n = do
  e <- createRowVector e
  r <- mlGenericFun mlg "eval" [anyMXArray e] n
  freeMXArray e
  return r

-- |Set the given variable to the value in the base scope
mlGenericSetVar :: MLGeneric -> String -> MXArray a -> IO ()
mlGenericSetVar mlg v x = do
  b <- createRowVector "base"
  v <- createRowVector v
  _ <- mlGenericFun mlg "assignin" [anyMXArray b,anyMXArray v,anyMXArray x] 0
  freeMXArray v
  freeMXArray b

-- |Get the value of the given variable in the base scope
mlGenericGetVar :: MLGeneric -> String -> IO MAnyArray
mlGenericGetVar mlg v = head =.< mlGenericEval mlg v 1

-- |Evaluate Matlab code and capture the generated output (see 'mlGenericEval')
mlGenericCapture :: MLGeneric -> String -> Int -> IO (String,[MAnyArray])
mlGenericCapture mlg e n = do
  e <- createRowVector e
  (cap:res) <- mlGenericFun mlg "evalc" [anyMXArray e] (succ n)
  freeMXArray e
  Just cap <- castMXArray cap
  cap <- mxArrayGetAll cap
  return $ (cap,res)
