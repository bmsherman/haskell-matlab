{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Test.Engine where

import Control.Exception (SomeException, assert, try)
import Data.Either (isLeft, isRight, lefts)
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
  testIsMNull eng
  testGetFirst eng
  testAbstractValueUse eng
  testTypedAbstractValueUse eng
  testGetByteStreamFromArray eng
  testGetArrayFromByteStream eng
  testCellGet eng
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

testIsMNull :: Engine -> IO ()
testIsMNull eng = do
  putStrLn $ "\n-- testIsMNull --"
  xa <- createMXScalar (1.0 :: MDouble)
  let xaRes = assert (isMNull xa == False) xa
  xaResEi <- mxArrayGetFirst xaRes
  putStrLn $ "  xaResEi is Right: " <> (show $ isRight xaResEi)
  xae :: MXArray MChar <- createMXArray []
  freeMXArray xae
  mxLen <- mxArrayLength xae
  putStrLn $ "length is " <> (show mxLen)
  -- This is a bit surprising, but ok
  let xaeRes = assert (isMNull xae == False) xae
  xaeResEi <- mxArrayGetFirst xaeRes
  putStrLn $ "  xaeResEi is Right: " <> (show $ isRight xaeResEi)

testGetFirst :: Engine -> IO ()
testGetFirst eng = do
  putStrLn $ "\n-- testGetFirst --"
  xa <- createMXScalar (1.0 :: MDouble)
  xEi <- mxArrayGetFirst xa
  let xRes = assert (xEi == Right 1.0) xEi
  putStrLn $ "  xRes is : " <> (show xRes)
  
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

-- TODO: display cell array and extracted values in test
testCellGet :: Engine -> IO ()
testCellGet eng = do
  putStrLn "\n-- testCellGet --"
  [ca] <- engineEvalFun eng "mcellTest" [] 1
  Just (ca :: MXArray MCell) <- castMXArray ca
  caLen <- mxArrayLength ca
  let caLenMsg = assert (caLen == 6) "cell array has length 6"
  putStrLn caLenMsg
  dCells :: [MXArray MDouble] <- mxCellGetArraysOfType ca
  let dCellsMsg = assert (length dCells == 4) "cell array has 4 double arrays"
  putStrLn dCellsMsg
  dVals :: [MDouble] <- mxCellGetAllOfType ca
  let dValsMsg = assert (length dVals == 4) "cell array has 4 double values"
  putStrLn dValsMsg

testClearVar :: Engine -> IO ()
testClearVar eng = do
  putStrLn $ "\n-- testClearVar --"
  let foopi = "foopi"
  x <- createMXScalar (pi :: MDouble)
  engineSetVar eng foopi x
  ei1 :: Either SomeException MAnyArray <- try $ engineGetVar eng foopi
  putStrLn $ assert (isRight ei1) "  Can clearVar once"
  clearVar eng foopi
  ei2 :: Either SomeException MAnyArray <- try $ engineGetVar eng foopi
  putStrLn $ assert (isLeft ei2) $
    " Can't clearVar twice: " <> (show $ lefts [ei2])
  putStrLn "  Finished testClearVar"


testRel :: Path Rel Dir
testRel = $(mkRelDir "test")

repoDir :: Path Abs Dir
repoDir = $(mkAbsDir getRepoDirStatic)
