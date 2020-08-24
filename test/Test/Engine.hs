{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Test.Engine where

import Control.Exception (SomeException, assert, try)
import Data.Either (isLeft, isRight, lefts)
import Data.List (intercalate)
import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers
import Foreign.Matlab.MAT
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (lift)
import Path
import Test.Util

engineTests = runEngineTests ""

runEngineTests :: String -> IO ()
runEngineTests host = do
  putStrLn " -- Non-engine tests --"
  testLoadFiles
  testGetFirstLast
  makeEmptyArrays
  putStrLn "-- Starting engine --"
  eng <- newEngine host
  putStrLn "-- Engine created --"
  let testPath = repoDir </> testRel
  addpath eng testPath
  runLocalMatFun eng
  cosOfPi eng
  testIsMNull eng
  testCreateRowVectorOfStructs eng
  testAbstractValueUse eng
  testTypedAbstractValueUse eng
  testGetByteStreamFromArray eng
  testGetArrayFromByteStream eng
  testCellGet eng
  testAssignMap eng
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
  putStrLn $ "  xaResEi is Right: " <> (show xaResEi)
  xae :: MXArray MChar <- createMXArray []
  freeMXArray xae
  mxLen <- mxArrayLength xae
  mxDims <- mxArraySize xae
  putStrLn $ "length is " <> (show mxLen) <> " dims are " <> (show $ mxDims)
  let xaeRes = assert (isMNull xae == False) xae
  xaeResEi <- mxArrayGetFirst xaeRes
  putStrLn $ "  xaeResEi is Left: " <> (show xaeResEi)

testGetFirstLast :: IO ()
testGetFirstLast = do
  putStrLn $ "\n-- testGetFirstLast --"
  let testVal :: MDouble = 1.0
  xa <- createMXScalar testVal
  xfEi <- mxArrayGetFirst xa
  xlEi <- mxArrayGetLast xa
  let xRes = assert (xlEi == Right 1.0 && xfEi == xlEi) xfEi
  putStrLn $ "  xRes is : " <> (show xRes)
  threeArray :: MXArray MDouble <- fromListIO [5.0, 6.0, 7.0]
  txfEi <- mxArrayGetFirst threeArray
  txlEi <- mxArrayGetLast threeArray
  let txfRes = assert (txfEi == Right 5.0) txfEi
  putStrLn $ "  txfRes is : " <> (show txfRes)
  let txlRes = assert (txlEi == Right 7.0) txlEi
  putStrLn $ "  txlRes is : " <> (show txlRes)
  
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

testCreateRowVectorOfStructs :: Engine -> IO ()
testCreateRowVectorOfStructs eng = do
  putStrLn $ "\n-- testCreateRowVectorOfStructs --"
  sOut <- makeTestStruct eng
  Just mxSA <- castMXArray sOut
  Right ms <- mxArrayGetFirst mxSA

  mxSA1 <- createRowVector [ms]
  msList1 :: [MStruct] <- mxArrayGetAll mxSA1
  putStrLn $ "length of msList1 is " <> (show $ length msList1)

  mxSA2 <- createRowVector [ms, ms, ms, ms]
  msList2 :: [MStruct] <- mxArrayGetAll mxSA2
  putStrLn $ "length of msList2 is " <> (show $ length msList2)

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

-- TODO: Shouldn't need engine for this
testLoadFiles :: IO ()
testLoadFiles = do
  let doublesPath = repoDir </> testRel </> doublesFile
  putStrLn $ "reading doubles file: " <> (toFilePath doublesPath)
  mdFile <- matOpen (toFilePath doublesPath) MATRead
  Just (mxDubsAA :: MAnyArray) <- matGet mdFile "fiveSquares"
  Just (mxDubs :: MXArray MDouble) <- castMXArray mxDubsAA
  dubs <- mxArrayGetAll mxDubs

  let cellSPath = repoDir </> testRel </> cellStringsFile
  putStrLn $ "reading cell strings file: " <> (toFilePath cellSPath)
  mcsFile <- matOpen (toFilePath cellSPath) MATRead
  Just (mxCSAA :: MAnyArray) <- matGet mcsFile "helloCell"
  Just (mxCS :: MXArray MCell) <- castMXArray mxCSAA
  cellStrings :: [String] <- mxCellGetAllListsOfType mxCS
  putStrLn $ intercalate " " cellStrings

makeEmptyArrays :: IO ()
makeEmptyArrays = do
  dubE <- fromListIO ([] :: [MDouble])
  putStrLn $ "isMNull dubE?:" <> (show $ isMNull dubE)
  dubs <- mxArrayGetAll dubE
  let dubLen_ = length dubs
  let dubLen = assert (dubLen_ == 0) dubLen_
  putStrLn $ "dubLen: " <> (show dubLen)
  dubsLastEi <- mxArrayGetLast dubE
  putStrLn $ "isRight dubsLastEi?: " <> (show $ isRight dubsLastEi)

  cellsE <- fromListIO ([] :: [MCell])
  putStrLn $ "isMNull cellsE?:" <> (show $ isMNull cellsE)
  cells <- mxArrayGetAll cellsE
  let cellLen_ = length cells
  let cellLen = assert (cellLen_ == 0) cellLen_
  putStrLn $ "cellLen: " <> (show cellLen)
  cellLastEi <- mxArrayGetLast cellsE
  putStrLn $ "isRight cellLastEi?: " <> (show $ isRight cellLastEi)

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


testAssignMap :: Engine -> IO ()
testAssignMap eng = do
  putStrLn "\n-- testAssignMap --"
  [mMap] <- engineEvalEngFun eng "mMapTest" [] 1
  [valuesAA] <- engineEvalFun eng "values" [EvalMEngVar $ mMap] 1
  Just valuesCA <- castMXArray valuesAA
  values :: [String] <- mxCellGetAllListsOfType valuesCA
  putStrLn $ show $ values


testRel :: Path Rel Dir
testRel = $(mkRelDir "test")

repoDir :: Path Abs Dir
repoDir = $(mkAbsDir getRepoDirStatic)

doublesFile :: Path Rel File
doublesFile = $(mkRelFile "fiveSquares.mat")

cellStringsFile :: Path Rel File
cellStringsFile = $(mkRelFile "helloCell.mat")