{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 0
#endif

#if (MIN_VERSION_profunctors(4,4,0)) && __GLASGOW_HASKELL__ >= 708
#define USE_COERCE
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
#else
{-# LANGUAGE Unsafe #-}
#endif

{- |
Module      :  Lens.Micro.Pro
Copyright   :  (C) 2013-2016 Edward Kmett, 2018 Monadfix
License     :  BSD-style (see the file LICENSE)

This module provides types and functions that require 'Profunctor'; they aren't included in the main microlens package because <http://hackage.haskell.org/package/profunctors profunctors> has a lot of dependencies.
-}
module Lens.Micro.Pro
(
  -- * Prisms

  -- * Isomorphisms
)
where


import Control.Monad.Reader
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Tagged
import Data.Functor.Identity
import Data.Maybe
import Lens.Micro

#ifdef USE_COERCE
import Data.Coerce
#else
import Unsafe.Coerce
#endif


----------------------------------------------------------------------------
-- Coerce shim
----------------------------------------------------------------------------

#ifdef USE_COERCE
coerce' :: forall a b. Coercible a b => b -> a
coerce' = coerce (id :: a -> a)
{-# INLINE coerce' #-}
#else
coerce, coerce' :: a -> b
coerce  = unsafeCoerce
coerce' = unsafeCoerce
{-# INLINE coerce #-}
{-# INLINE coerce' #-}
#endif

----------------------------------------------------------------------------
-- Prisms
----------------------------------------------------------------------------

type Prism s t a b =
  forall p f. (Choice p, Applicative f)
  => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

----------------------------------------------------------------------------
-- Isomorphisms
----------------------------------------------------------------------------

type Iso s t a b =
  forall p f. (Profunctor p, Functor f)
  => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce'
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

-- | Extract the two functions, one from @s -> a@ and
-- one from @b -> t@ that characterize an 'Iso'.
withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)
{-# INLINE withIso #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

from :: Iso s t a b -> Iso b a t s
from l = withIso l $ \ sa bt -> iso bt sa
{-# INLINE from #-}

enum :: Enum a => Iso' Int a
enum = iso toEnum fromEnum
{-# INLINE enum #-}

-- | This can be used to lift any 'Iso' into an arbitrary 'Functor'.
mapping :: (Functor f, Functor g) => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \ sa bt -> iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

non :: Eq a => a -> Iso' (Maybe a) a
non a = non' (only a)
{-# INLINE non #-}

-- TODO: (non' . only) is broken because of using Prism' instead of APrism'

non' :: Prism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                           = review p ()
  go b | has p b                = Nothing
       | otherwise              = Just b
{-# INLINE non' #-}

only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
{-# INLINE only #-}

type AReview t b = Tagged b (Identity b) -> Tagged t (Identity t)

review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}

#if __GLASGOW_HASKELL__ >= 708
-- | Data types that are representationally equal are isomorphic.
--
-- This is only available on GHC 7.8+
coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
# if __GLASGOW_HASKELL__ >= 710
coerced l = rmap (fmap coerce') l .# coerce
# else
coerced l = case sym Coercion :: Coercion a s of
              Coercion -> rmap (fmap coerce') l .# coerce
# endif
{-# INLINE coerced #-}
#endif
