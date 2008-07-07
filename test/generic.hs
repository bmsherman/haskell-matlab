import Foreign.Matlab
import Foreign.Matlab.Runtime.Generic

main = do
  ml <- openMLGeneric ["-nojvm", "-nojit"]
  putStrLn "ready"
  x <- createMXScalar (pi :: MDouble)
  [y] <- mlGenericFun ml "cos" [anyMXArray x] 1
  mxArrayClass y >>= print
  Just y <- castMXArray y
  y <- mxScalarGet y
  print (y :: MDouble)
  closeMLGeneric ml
