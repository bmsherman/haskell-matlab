{-|
  Interface to Matlab runtime libraries.
  This uses a Matlab shared library which has been built with \"mcc -l\", and only functions in this library may be called.
  Multiple libraries may be loaded simultaneously.

  Note that you cannot use "Foreign.Matlab.Runtime" and "Foreign.Matlab.Engine" in the same program.
  This seems to be a Matlab limitation.
-}
module Foreign.Matlab.Runtime (
    MLibrary,
    openMLibrary,
    closeMLibrary,
    mLibraryFun,
    mLibraryCall,

    mlGenericFeval
  ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import Foreign.C.Types
import System.Posix.DynamicLinker
import Data.List
import qualified Data.Char
import Distribution.Simple.BuildPaths (dllExtension)
import Control.Concurrent.MVar
import System.FilePath (splitFileName, dropExtensions, extSeparator
  , (<.>), (</>))
import Foreign.Matlab.Util
import Foreign.Matlab.Internal

import System.IO.Unsafe (unsafePerformIO)

#include "hsc_sym.h"
#include "libhsmatlab.h"

initialized :: MVar Integer
initialized = unsafePerformIO (newMVar 0)

type InitApp = Ptr CString -> CInt -> IO CBool
type TermApp = IO CBool

initializeApp :: InitApp -> [String] -> IO ()
initializeApp init opt = modifyMVar_ initialized maybeinit where
  maybeinit 0 = 
    mapWithArrayLen withCString opt $ \(optp,optn) -> do
    r <- init optp (ii optn)
    if boolC r
      then return 1
      else fail "mclInitializeApplication"
  maybeinit n = return $ succ n

terminateApp :: TermApp -> IO ()
terminateApp term = modifyMVar_ initialized maybeterm where
  maybeterm 1 = do
    r <- term
    if boolC r
      then return 0
      else fail "mclTerminateApplication"
  maybeterm n = return $ pred n

{-
foreign import ccall unsafe mclInitializeApplication :: InitApp
foreign import ccall unsafe mclTerminateApplication :: TermApp
initialize = initializeApp mclInitializeApplication
terminate = terminateApp mclTerminateApplication
-}

foreign import ccall "dynamic" mkInitApp :: FunPtr InitApp -> InitApp
foreign import ccall "dynamic" mkTermApp :: FunPtr TermApp -> TermApp

type InitFun = IO CBool
type FiniFun = IO ()
type MLXFun = CInt -> Ptr MXArrayPtr -> CInt -> Ptr MXArrayPtr -> IO CBool

foreign import ccall "dynamic" mkInitFun :: FunPtr InitFun -> InitFun
foreign import ccall "dynamic" mkFiniFun :: FunPtr FiniFun -> FiniFun
foreign import ccall "dynamic" mkMLXFun :: FunPtr MLXFun -> MLXFun

-- |A Matlab library handle
data MLibrary = MLibrary { mlName :: String, mlDL :: DL }

-- |Open and initialize a matlab shared library.
openMLibrary :: 
  String  -- ^ The name of the library, which may be a full path to the file, or simply the library name
  -> [String] -- ^ Arguments with which to initialize the application instance (e.g., \"-nojvm\")
  -> IO MLibrary
openMLibrary mlname opt = do
  let (path, base) = splitFileName mlname
      name = (if isPrefixOf "lib" base then drop 3 else id) (dropExtensions base)
      file = (if isPrefixOf "lib" base then id else ("lib" ++)) ((if isSuffixOf dllExtension base || isInfixOf (dllExtension++[extSeparator]) base then id else (<.> dllExtension)) base)
  dl <- dlopen (path </> file) [RTLD_NOW]
  let ml = MLibrary name dl
  inia <- mkInitApp =.< dlsym dl #SYM mclInitializeApplication
  initializeApp inia opt
  --initialize opt
  inif <- mkInitFun =.< dlsym dl ("lib" ++ name ++ "Initialize")
  r <- inif
  if boolC r
    then return ml
    else fail ("lib" ++ name ++ "Initialize")

-- |Terminate and close a matlab library.
closeMLibrary :: MLibrary -> IO ()
closeMLibrary (MLibrary name dl) = do
  fini <- mkFiniFun =.< dlsym dl ("lib" ++ name ++ "Terminate")
  fini
  fina <- mkTermApp =.< dlsym dl #SYM mclTerminateApplication
  terminateApp fina
  --terminate
  dlclose dl

makeMFun :: MLXFun -> MFun
makeMFun fun arg no =
  mapWithArrayLen withMXArray arg $ \(argp,argn) ->
  allocaArray no $ \outp -> do
  r <- fun (ii no) outp (ii argn) argp
  if boolC r
    then peekArray no outp >>= mapM mkMXArray
    else fail "MFun"

-- |Return a Haskell function representing the Matlab function with the given name in the given library
mLibraryFun :: MLibrary -> String -> IO MFun
mLibraryFun (MLibrary _ dl) fun =
  (makeMFun . mkMLXFun) =.< dlsym dl ("mlx" ++ Data.Char.toUpper (head fun) : tail fun)

-- |Call the Matlab function with the given name in the given library directly
mLibraryCall :: MLibrary -> String -> MFun
mLibraryCall ml f arg no = do
  fun <- mLibraryFun ml f
  fun arg no

foreign import ccall "dynamic" mkFeval :: FunPtr (CString -> MLXFun) -> CString -> MLXFun

-- |Internal use only.  See "Foreign.Matlab.Runtime.Generic"
mlGenericFeval :: MLibrary -> IO (CString -> MFun)
mlGenericFeval MLibrary{ mlName = "hsmatlab", mlDL = dl } = do
  fe <- mkFeval =.< dlsym dl "mlHsFeval"
  return $ makeMFun . fe
mlGenericFeval _ = fail "mlGenericEval: use Matlab.Runtime.Generic"
