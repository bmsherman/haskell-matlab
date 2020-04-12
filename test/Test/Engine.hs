{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Test.Engine where

import Foreign.Matlab
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (lift)
import Path
import Test.Util

engineTests = runEngineTests ""

runEngineTests :: String -> IO ()
runEngineTests host = do
  putStrLn " -- Starting engine --"
  eng <- newEngine host
  putStrLn " -- Engine created --"
  runLocalMatFun eng
  cosOfPi eng

cosOfPi :: Engine -> IO ()
cosOfPi eng = do
  putStrLn "\n-- cos pi --"
  x <- createMXScalar (pi :: MDouble)
  cosBody eng "cos" x

runLocalMatFun :: Engine -> IO ()
runLocalMatFun eng = do
  putStrLn "\n-- mtest: cos pi --"
  let testPath = repoDir </> testRel
  x <- createMXScalar (pi :: MDouble)
  addpath eng testPath
  -- let addTestPath = "addpath " <> (toFilePath testPath)
  -- putStrLn $ "evaluating: " <> addTestPath
  -- engineEval eng addTestPath
  cosBody eng "mtest" x

cosBody :: Engine -> String -> MXArray MDouble -> IO ()
cosBody eng cosFun x = do
  [y] <- engineEvalFun eng cosFun [EvalArray x] 1
  mxArrayClass y >>= print
  Just y <- castMXArray y
  y <- mxScalarGet y
  print (y :: MDouble)

testRel :: Path Rel Dir
testRel = $(mkRelDir "test")

repoDir :: Path Abs Dir
repoDir = $(mkAbsDir getRepoDirStatic)
