import Foreign.Matlab
import Foreign.Matlab.Engine

main = do
  eng <- newEngine Nothing
  putStrLn "ready"
  x <- createMXScalar (pi :: MDouble)
  [y] <- engineEvalFun eng "cos" [EvalArray x] 1
  mxArrayClass y >>= print
  Just y <- castMXArray y
  y <- mxScalarGet y
  print (y :: MDouble)
