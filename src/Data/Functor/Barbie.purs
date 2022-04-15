{-|
Module     : Data.Functor.Barbie
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2020-2022 Purescript QuickCheck State Machine
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Data.Functor.Barbie where

import Control.Alt (void)
import Control.Alternative (class Applicative)
import Control.Category ((<<<))
import Control.Monad.Writer (execWriter)
import Control.Monad.Writer as Writer
import Data.Const (Const(..))
import Data.Function (const)
import Data.Functor as Rank1
import Data.Monoid (class Monoid)
import Data.Unit (Unit, unit)

class Functor :: forall k. ((k -> Type) -> Type) -> Constraint
class Functor barbie where
    map :: forall f g. (forall a. f a -> g a) -> barbie f -> barbie g

class Traversable :: forall k. ((k -> Type) -> Type) -> Constraint
class Functor barbie <= Traversable barbie where
    traverse :: forall f g context. Applicative context => (forall a. f a -> context (g a)) -> barbie f -> context (barbie g)

traverse_ :: forall barbie context f b. Traversable barbie => Applicative context => (forall a. f a -> context b) -> barbie f -> context Unit
traverse_ f = void <<< traverse (Rank1.map (const (Const unit)) <<< f)

foldMap :: forall barbie f a. Traversable barbie => Monoid a => (forall b. f b -> a) -> barbie f -> a
foldMap f = execWriter <<< traverse_ (Writer.tell <<< f)
