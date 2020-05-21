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
  putStrLn "-- Starting engine --"
  eng <- newEngine host
  putStrLn "-- Engine created --"
  let testPath = repoDir </> testRel
  addpath eng testPath
  runLocalMatFun eng
  cosOfPi eng
  testAbstractValueUse eng
  testTypedAbstractValueUse eng

cosOfPi :: Engine -> IO ()
cosOfPi eng = do
  putStrLn "\n-- cos pi --"
  x <- createMXScalar (pi :: MDouble)
  cosBody eng "cos" x

runLocalMatFun :: Engine -> IO ()
runLocalMatFun eng = do
  putStrLn "\n-- mtest: cos pi --"
  x <- createMXScalar (pi :: MDouble)
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

testAbstractValueUse :: Engine -> IO ()
testAbstractValueUse eng = do
  putStrLn $ "\n-- testAbstractValueUse -- "
  sOut <- makeTestStruct eng
  sSum <- useTestStruct eng sOut
  putStrLn $ "  struct sum is: " <> (show sSum)

makeTestStruct :: Engine -> IO MAnyArray
makeTestStruct eng = do
  [res] <- engineEvalFun eng "makeTestStruct" [] 1
  pure res

useTestStruct :: Engine -> MAnyArray -> IO MDouble
useTestStruct eng sIn = do
  [res] <- engineEvalFun eng "useTestStruct" [EvalArray sIn] 1
  mxArrMay <- castMXArray res
  case mxArrMay of
    Just mxArr -> mxScalarGet mxArr
    Nothing -> pure 0.0


newtype MyAbsType = MyAbsType { unMyAbsType :: MAnyArray }

-- |Similar to testAbstractValueUse, but instead of using
-- |MAnyArray, we use newtypes for better type safety
testTypedAbstractValueUse :: Engine -> IO ()
testTypedAbstractValueUse eng = do
  putStrLn $ "\n-- testTypedAbstractValueUse -- "
  sOut <- makeTestStructTyped eng
  sSum <- useTestStructTyped eng sOut
  putStrLn $ "  struct sum is: " <> (show sSum)

makeTestStructTyped :: Engine -> IO MyAbsType
makeTestStructTyped eng = MyAbsType <$> (makeTestStruct eng)

useTestStructTyped :: Engine -> MyAbsType -> IO MDouble
useTestStructTyped eng (MyAbsType sIn) = useTestStruct eng sIn

testRel :: Path Rel Dir
testRel = $(mkRelDir "test")

repoDir :: Path Abs Dir
repoDir = $(mkAbsDir getRepoDirStatic)
