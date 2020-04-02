module Test.Engine where

import Foreign.Matlab
import Foreign.Matlab.Engine

-- import System.FilePath
-- import Turtle (decodeString, encodeString)
-- import Turtle.Prelude (which)

-- engineTests = do
--   matPathMay <- which (decodeString "matlab")
--   case matPathMay of
--     Just matPath -> runEngineTests (encodeString matPath)
--     Nothing -> putStrLn "'matlab' executable not on PATH!"

engineTests = runEngineTests ""

runEngineTests :: String -> IO ()
runEngineTests host = do
  eng <- newEngine host
  putStrLn "ready"
  x <- createMXScalar (pi :: MDouble)
  [y] <- engineEvalFun eng "cos" [EvalArray x] 1
  mxArrayClass y >>= print
  Just y <- castMXArray y
  y <- mxScalarGet y
  print (y :: MDouble)
