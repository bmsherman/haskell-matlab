{-|
  Read and write MAT-format Matlab data files.
-}
module Foreign.Matlab.MAT (
    MATFile,
    MATMode(..),
    matOpen,
    matSet,
    matGet,
    matRemove,
    matList,
    -- ** Convenience whole-file operations
    matLoad,
    matSave
  ) where

import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import Foreign.Matlab.Util
import Foreign.Matlab.Internal
import Foreign.Matlab.Types

#include <mat.h>

data MATFileType
type MATFilePtr = Ptr MATFileType
-- |The opaque type of MAT file handles
newtype MATFile = MATFile MATFilePtr

withMATFile :: With MATFile MATFilePtr (IO a)
withMATFile (MATFile mat) f = f mat

foreign import ccall unsafe "matOpen" matOpen_c :: CString -> CString -> IO MATFilePtr
foreign import ccall unsafe "matClose" matClose_c :: MATFilePtr -> IO CInt

data MATMode = MATRead | MATWrite | MATUpdate

-- |Open a MAT-file using mode.
matOpen :: FilePath -> MATMode -> IO MATFile
matOpen f m = do 
  throwErrnoIfNull ("matOpen: " ++ f)
    (withCString f (withCString (ms m) . matOpen_c))
    >.= MATFile
  where
    ms MATRead = "r"
    ms MATWrite = "w"
    ms MATUpdate = "u"

-- |Close a MAT-file opened with matOpen.
matClose :: MATFile -> IO ()
matClose m = throwErrnoIfMinus1_ "matClose" $ withMATFile m matClose_c

foreign import ccall unsafe matPutVariable :: MATFilePtr -> CString -> MXArrayPtr -> IO CInt
foreign import ccall unsafe matPutVariableAsGlobal :: MATFilePtr -> CString -> MXArrayPtr -> IO CInt
-- |Write array value with the specified name to the MAT-file, deleting any previously existing variable with that name in the MAT-file.
matSet :: MATFile 
  -> Bool -- ^ Global. If true, the variable will be written such that when the MATLAB LOAD command loads the variable, it will automatically place it in the global workspace.
  -> String -> MXArray a -> IO ()
matSet m g n v = do
  r <- withMATFile m (\m -> withCString n (withMXArray v . (if g then matPutVariableAsGlobal else matPutVariable) m))
  when (r /= 0) $ fail "matPut"

foreign import ccall unsafe matGetVariable :: MATFilePtr -> CString -> IO MXArrayPtr
-- |Read the array value for the specified variable name from a MAT-file.
matGet :: MATFile -> String -> IO (Maybe MAnyArray)
matGet m n = do
  a <- withMATFile m (withCString n . matGetVariable) 
  if a == nullPtr
    then return Nothing
    else Just =.< mkMXArray a

foreign import ccall unsafe matDeleteVariable :: MATFilePtr -> CString -> IO CInt
-- |Remove a variable with with the specified name from the MAT-file.
matRemove :: MATFile -> String -> IO ()
matRemove m n = do
  r <- withMATFile m (withCString n . matDeleteVariable)
  when (r /= 0) $ fail "matRemove"

foreign import ccall unsafe mxFree :: Ptr a -> IO ()

foreign import ccall unsafe matGetDir :: MATFilePtr -> Ptr CInt -> IO (Ptr CString)
-- |Get a list of the names of the arrays in a MAT-file.
matList :: MATFile -> IO [String]
matList m =
  withMATFile m $ \m -> alloca $ \n -> do
  sp <- matGetDir m n
  n <- peek n
  when (n < 0) $ fail "matList"
  s <- mapM peekCString =<< peekArray (ii n) sp
  mxFree sp
  return s

foreign import ccall unsafe matGetNextVariable :: MATFilePtr -> Ptr CString -> IO MXArrayPtr
-- |Load all the variables from a MAT file
matLoad :: FilePath -> IO [(String,MAnyArray)]
matLoad file = do
  mat <- matOpen file MATRead
  vars <- withMATFile mat load
  matClose mat
  return vars
  where
    load m =
      alloca $ \n -> do
      a <- matGetNextVariable m n
      if a == nullPtr then return [] else do
      a <- mkMXArray a
      n <- peek n >>= peekCString
      ((n,a) :) =.< load m

-- |Write all the variables to a new MAT file
matSave :: FilePath -> [(String,MXArray a)] -> IO ()
matSave file vars = do
  mat <- matOpen file MATWrite
  mapM_ (uncurry $ matSet mat False) vars
  matClose mat
