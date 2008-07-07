import Foreign.Matlab
import Foreign.Matlab.Runtime

main = do
  ml <- openMLibrary "./mtest" ["-nojvm", "-nojit"]
  putStrLn "ready"
  x <- createMXScalar (pi :: MDouble)
  [y] <- mLibraryCall ml "mtest" [anyMXArray x] 1
  mxArrayClass y >>= print
  Just y <- castMXArray y
  y <- mxScalarGet y
  print (y :: MDouble)
  closeMLibrary ml
