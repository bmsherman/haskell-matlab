import Control.Monad
import Foreign.Matlab
import Foreign.Matlab.Runtime.Generic

loop ml = do
  i <- getLine
  when (i /= "") $ do
    mlGenericEval ml i 0
    loop ml

main = do
  ml <- openMLGeneric ["-nojvm", "-nojit"]
  putStrLn "ready. enter matlab expression or blank line to exit."
  loop ml
  closeMLGeneric ml
