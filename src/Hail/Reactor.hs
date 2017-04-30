{-# LANGUAGE RankNTypes #-}
module Hail.Reactor
  ( Reactor(..)
  , hoist
  ) where

import Data.ByteString (ByteString)
import Data.Semigroup (Semigroup(..))
import Hail.Event (Event)

newtype Reactor m = Reactor (Event ByteString -> m ())

instance Applicative m => Semigroup (Reactor m) where
  Reactor a <> Reactor b = Reactor $ (*>) <$> a <*> b

instance Applicative m => Monoid (Reactor m) where
  mempty = Reactor . const . pure $ ()
  mappend = (<>)

hoist :: (forall a. m a -> n a) -> Reactor m -> Reactor n
hoist f (Reactor a) = Reactor $ f . a
