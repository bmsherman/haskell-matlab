import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.ModuleName (components)
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath

defhooks = simpleUserHooks

programs = [ simpleProgram "mcc" ]

runtime desc = maybe False (elem ["Foreign","Matlab","Runtime"] 
  . map components . exposedModules) $ library desc

build desc binfo hooks flags = do
  when (runtime desc) $ 
    rawSystemExit (fromFlag $ buildVerbosity flags) "make" ["-Csrc"]
  buildHook defhooks desc binfo hooks flags

clean desc binfo hooks flags = do
  makeExists <- doesFileExist "src/Makefile"
  when makeExists $ 
    rawSystemExit (fromFlag $ cleanVerbosity flags) "make" ["-Csrc", "clean"]
  cleanHook defhooks desc binfo hooks flags

install desc binfo hooks flags = do
  instHook defhooks desc binfo hooks flags
  when (runtime desc) $ mapM_ (\f -> 
      copyFileVerbose (fromFlag $ installVerbosity flags) 
	("src" </> f) 
	(libdir (absoluteInstallDirs desc binfo NoCopyDest) </> f))
    ["libhsmatlab.so"]

reg desc binfo hooks flags = do
  pwd <- getCurrentDirectory
  let
    desc' = desc{ library = fmap lm (library desc) }
    lm l = l { libBuildInfo = (libBuildInfo l)
	{ ldOptions = map ("-Wl,-rpath," ++) (lib : extraLibDirs (libBuildInfo l) )
                      ++ ldOptions (libBuildInfo l),
          extraLibDirs = (pwd </> "src") : extraLibDirs (libBuildInfo l)  } }
    lib
      | fromFlag $ regInPlace flags = pwd </> "src"
      | otherwise = libdir (absoluteInstallDirs desc binfo NoCopyDest)
  regHook defhooks desc' binfo hooks flags

hooks = defhooks {
    hookedPrograms = programs,
    buildHook = build,
    cleanHook = clean,
    instHook = install,
    regHook = reg
  }

main = defaultMainWithHooks hooks
