module Foreign.Matlab.Util where

import Control.Monad
import Foreign
import Path

infixl 1 >., >.=, >=.
infixr 1 =.<, .=<
(>.) :: Monad m => m a -> b -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b
(>=.) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(.=<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c

(>.) e r = e >> return r
(>.=) e r = e >>= return . r
(=.<) r e = return . r =<< e
(>=.) e r = e >=> return . r
(.=<) r e = return . r <=< e

ii :: (Integral a, Integral b) => a -> b
ii = fromIntegral

type With x y a = x -> (y -> a) -> a

mapWith :: With x y a -> With [x] [y] a
mapWith w l f = m l id where
  m [] s = f (s [])
  m (x:l) s = w x (\y -> m l (s.(y:)))

mapWith' :: With x y a -> With [x] [y] a
mapWith' w = m where
  m [] e = e []
  m (x:l) e = m l (\s -> w x (\y -> e (y:s)))

mapWithRev :: With x y a -> With [x] [y] a
mapWithRev w l f = m l [] where
  m [] s = f s
  m (x:l) s = w x (\y -> m l (y:s))

mapWithArray :: Storable y => With x y (IO a) -> With [x] (Ptr y) (IO a)
mapWithArray w l f = mapWithArrayLen w l (\(p,_) -> f p)

mapWithArrayLen :: Storable y => With x y (IO a) -> With [x] (Ptr y, Int) (IO a)
--mapWithArrayLen w l f = mapWith w l (\l -> withArrayLen l (curry f))
mapWithArrayLen w l f =
  allocaArray (length l) $ \p ->
    let
      set [] i = f (p,i)
      set (x:l) i = w x $ \y -> pokeElemOff p i y >> set l (succ i)
    in set l 0

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment n l = a : segment n r where (a,r) = splitAt n l

replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex [] _ _ = error "replaceIndex: index too large"
replaceIndex (_:l) 0 y = y:l
replaceIndex (x:l) n y = x : replaceIndex l (pred n) y


fromFile :: Path b File -> FilePath
fromFile = toFilePath

fromDir :: Path b Dir -> FilePath
fromDir = toFilePath
