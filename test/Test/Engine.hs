{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Test.Engine where

import Control.Exception (assert)
import Foreign.Matlab
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers
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
  testGetByteStreamFromArray eng
  testGetArrayFromByteStream eng
  testClearVar eng

cosOfPi :: Engine -> IO ()
cosOfPi eng = do
  putStrLn "\n-- cos pi --"
  x <- createMXScalar (pi :: MDouble)
  cosBody eng "cos" x

runLocalMatFun :: Engine -> IO ()
runLocalMatFun eng = do
  putStrLn "\n-- mtest: cos pi --"
  x <- createMXScalar (pi :: MDouble)
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
  putStrLn $ "\n-- testAbstractValueUse --"
  sOut <- makeTestStruct eng
  sSum <- useTestStruct eng sOut
  let sSumRes = assert (sSum == 7.0) sSum
  putStrLn $ "  struct sum is: " <> (show sSumRes)

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
  putStrLn $ "\n-- testTypedAbstractValueUse --"
  sOut <- makeTestStructTyped eng
  sSum <- useTestStructTyped eng sOut
  let sSumRes = assert (sSum == 7.0) sSum
  putStrLn $ "  struct sum is: " <> (show sSumRes)

makeTestStructTyped :: Engine -> IO MyAbsType
makeTestStructTyped eng = MyAbsType <$> (makeTestStruct eng)

useTestStructTyped :: Engine -> MyAbsType -> IO MDouble
useTestStructTyped eng (MyAbsType sIn) = useTestStruct eng sIn

testGetByteStreamFromArray :: Engine -> IO ()
testGetByteStreamFromArray eng = do
  putStrLn $ "\n-- testGetByteStreamFromArray --"
  sOutBSMatlab <- makeTestStructByteStream eng
  sOut <- makeTestStruct eng
  Right sOutBSHaskell <- getByteStreamFromArray eng sOut
  let bsSum = sum $ fromIntegral <$> (assert (sOutBSMatlab == sOutBSHaskell) sOutBSHaskell)
  putStrLn $ " bytestream sum is: " <> (show bsSum)

testGetArrayFromByteStream :: Engine -> IO ()
testGetArrayFromByteStream eng = do
  putStrLn $ "\n-- testGetArrayFromByteStream --"
  sOutBS <- makeTestStructByteStream eng
  sOutFromBS <- getArrayFromByteStream eng sOutBS
  sOut <- makeTestStruct eng
  sSumFromBS <- useTestStruct eng sOutFromBS
  sSum <- useTestStruct eng sOut
  let sSumRes = assert (sSumFromBS == sSum) sSumFromBS
  putStrLn $ " deserialized struct sum is: " <> (show sSumRes)

makeTestStructByteStream :: Engine -> IO [MUint8]
makeTestStructByteStream eng = do
  [res] <- engineEvalFun eng "makeTestStructByteStream" [] 1
  mxArrMay <- castMXArray res
  case mxArrMay of
    Just mxArr -> mxArrayGetAll mxArr
    Nothing -> pure []

testClearVar :: Engine -> IO ()
testClearVar eng = do
  let foopi = "foopi"
  x <- createMXScalar (pi :: MDouble)
  engineSetVar eng foopi x
  x1 <- engineGetVar eng foopi
  clearVar eng foopi
  x2 <- engineGetVar eng foopi
  pure ()

testRel :: Path Rel Dir
testRel = $(mkRelDir "test")

repoDir :: Path Abs Dir
repoDir = $(mkAbsDir getRepoDirStatic)
