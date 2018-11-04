{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Lens.Micro.Pro.Internal
Copyright   :  (C) 2012-2016 Edward Kmett; 2018 Monadfix
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.Pro.Internal
(
  Iso, Iso',
  Prism, Prism',
  SimpleReview,
)
where

import Data.Profunctor
import Data.Bifunctor
import Data.Functor.Identity

type Iso s t a b =
  forall p f. (Profunctor p, Functor f)
  => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

type Prism s t a b =
  forall p f. (Choice p, Applicative f)
  => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type SimpleReview t b =
  forall p. (Choice p, Bifunctor p)
  => p b (Identity b) -> p t (Identity t)
