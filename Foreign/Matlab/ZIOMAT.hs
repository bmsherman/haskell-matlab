{-|
  Read and write MAT-format Matlab data files.
-}
module Foreign.Matlab.ZIOMAT (
    MM.MATFile,
    MM.MATMode(..),
    matClose,
    matOpen,
    matSet,
    matGet,
    matRemove,
    matList,
    -- ** Convenience whole-file operations
    matLoad,
    matSave
  ) where

import qualified Foreign.Matlab.MAT as MM
import           Foreign.Matlab.Internal
import           Foreign.Matlab.Util
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans

-- |Open a MAT-file using mode.
matOpen :: Path b File -> MM.MATMode -> ZIO r MatlabException MM.MATFile
matOpen f m = mxreZ . zlift $ MM.matOpen (fromFile f) m

-- |Close a MAT-file opened with matOpen.
matClose :: MM.MATFile -> ZIO r MatlabException ()
matClose = mxreZ . zlift . MM.matClose


-- |Write array value with the specified name to the MAT-file, deleting any previously existing variable with that name in the MAT-file.
matSet :: MM.MATFile
  -> Bool -- ^ Global. If true, the variable will be written such that when the MATLAB LOAD command loads the variable, it will automatically place it in the global workspace.
  -> String -> MXArray a -> ZIO r MatlabException ()
matSet m g n v = mxreZ . zlift $ MM.matSet m g n v

-- |Read the array value for the specified variable name from a MAT-file.
matGet :: MM.MATFile -> String -> ZIO r MatlabException MAnyArray
matGet m n = do
  aMay <- mxreZ . zlift $ MM.matGet m n
  case aMay of
    Just arr -> pure arr
    Nothing -> throwError MXNothing

-- |Remove a variable with with the specified name from the MAT-file.
matRemove :: MM.MATFile -> String -> ZIO r MatlabException ()
matRemove m n = mxreZ . zlift $ MM.matRemove m n

-- |Get a list of the names of the arrays in a MAT-file.
matList :: MM.MATFile -> ZIO r MatlabException [String]
matList = mxreZ . zlift . MM.matList

-- |Load all the variables from a MAT file
matLoad :: Path b File -> ZIO r MatlabException [(String,MAnyArray)]
matLoad f = mxreZ . zlift $ MM.matLoad (fromFile f)

-- |Write all the variables to a new MAT file
matSave :: Path b File -> [(String,MXArray a)] -> ZIO r MatlabException ()
matSave file vars = mxreZ . zlift $ MM.matSave (fromFile file) vars
