{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A few Optics definitions used by this project. These definitions are either
-- | copied or copmatible with those from the `lens` package.
module Foreign.Matlab.Optics where

import           Data.Coerce (Coercible, coerce)
import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a


-- | Build a 'Lens' from a getter and a setter.
--
-- @
-- 'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- @
--
-- >>> s ^. lens getter setter
-- getter s
--
-- >>> s & lens getter setter .~ b
-- setter s b
--
-- >>> s & lens getter setter %~ f
-- setter s (f (getter s))
--
-- @
-- 'lens' :: (s -> a) -> (s -> a -> s) -> 'Lens'' s a
-- @
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

coerce' :: forall a b. Coercible a b => b -> a
coerce' = coerce (id :: a -> a)
{-# INLINE coerce' #-}

coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
# if __GLASGOW_HASKELL__ >= 710
coerced l = rmap (fmap coerce') l .# coerce
# else
coerced l = case sym Coercion :: Coercion a s of
              Coercion -> rmap (fmap coerce') l .# coerce
# endif

