{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

#if (MIN_VERSION_profunctors(4,4,0)) && __GLASGOW_HASKELL__ >= 708
#define USE_COERCE
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
#else
{-# LANGUAGE Unsafe #-}
#endif

{- |
Module      :  Lens.Micro.Pro.Internal
Copyright   :  (C) 2012-2016 Edward Kmett; 2018 Monadfix
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.Pro.Internal
(
  -- * Iso
  Iso, Iso',
  iso,
  Exchange(..),

  -- * Prism
  Prism, Prism',
  prism,
  Market(..),

  -- * Review
  SimpleReview,
  unto,

  -- * Coerce compatibility shim
  coerce,
  coerce',
)
where

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Bifunctor
import Data.Functor.Identity
import Data.Void

#ifdef USE_COERCE
import Data.Coerce
#else
import Unsafe.Coerce
#endif

----------------------------------------------------------------------------
-- Iso
----------------------------------------------------------------------------

type Iso s t a b =
  forall p f. (Profunctor p, Functor f)
  => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

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

----------------------------------------------------------------------------
-- Prism
----------------------------------------------------------------------------

type Prism s t a b =
  forall p f. (Choice p, Applicative f)
  => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}
  lmap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE lmap #-}
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE rmap #-}
  ( #. ) _ = coerce'
  {-# INLINE ( #. ) #-}
  ( .# ) p _ = coerce p
  {-# INLINE ( .# ) #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  {-# INLINE left' #-}
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE right' #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

----------------------------------------------------------------------------
-- Review
----------------------------------------------------------------------------

type SimpleReview t b =
  forall p. (Choice p, Bifunctor p)
  => p b (Identity b) -> p t (Identity t)

unto :: (Profunctor p, Bifunctor p, Functor f)
     => (b -> t) -> p a (f b) -> p s (f t)
unto f = first absurd . lmap absurd . rmap (fmap f)
{-# INLINE unto #-}

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
