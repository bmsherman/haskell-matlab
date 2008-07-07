import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath
defhooks = defaultUserHooks
programs = [
    simpleProgram "matlab", 
    Program "mcr" (\_ -> return Nothing) (\_ _ -> return Nothing)
  ]
runtime desc = maybe False (elem "Foreign.Matlab.Runtime" . exposedModules) $ library desc
postconf args flags desc build = do
  confExists <- doesFileExist "configure"
  unless confExists $ rawSystemPathExit verb "autoconf" []
  postConf defhooks args flags{ configConfigureArgs = configConfigureArgs flags ++ confargs } desc build
  where 
    verb = configVerbose flags
    confargs = ("--" ++ (if runtime desc then "enable" else "disable") ++ "-runtime") : map pconfarg pconf
    pconfarg p = "--with-" ++ programId p ++ "=" ++ programPath p ++ " " ++ unwords (programArgs p)
    pconf = mapMaybe (\p -> lookupProgram p (withPrograms build)) programs
build desc binfo hooks flags = do
  when (runtime desc) $ rawSystemPathExit (buildVerbose flags) "make" ["-Csrc"]
  buildHook defhooks desc binfo hooks flags
clean desc binfo hooks flags = do
  makeExists <- doesFileExist "src/Makefile"
  when makeExists $ rawSystemPathExit (cleanVerbose flags) "make" ["-Csrc", "clean"]
  cleanHook defhooks desc binfo hooks flags
install desc binfo hooks flags = do
  instHook defhooks desc binfo hooks flags
  when (runtime desc) $ mapM_ (\f -> 
      copyFileVerbose (installVerbose flags) 
	("src" </> f) 
	(libdir (absoluteInstallDirs desc binfo NoCopyDest) </> f))
    ["libhsmatlab.so","libhsmatlab.ctf"]
reg desc binfo hooks flags = do
  pwd <- getCurrentDirectory
  let
    desc' = desc{ library = fmap lm (library desc) }
    lm l = l{ libBuildInfo = (libBuildInfo l)
	{ ldOptions = ("-Wl,-rpath," ++ lib) : ldOptions (libBuildInfo l) } }
    lib
      | regInPlace flags = pwd </> "src"
      | otherwise = libdir (absoluteInstallDirs desc binfo NoCopyDest)
  regHook defhooks desc' binfo hooks flags
hooks = defhooks {
    hookedPrograms = programs,
    postConf = postconf,
    buildHook = build,
    cleanHook = clean,
    instHook = install,
    regHook = reg
  }
main = defaultMainWithHooks hooks
