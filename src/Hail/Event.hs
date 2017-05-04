{-# LANGUAGE DeriveFunctor #-}
module Hail.Event
  ( Event(..)
  , Source(..)
  ) where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

data Event a = Event
  { eventTime :: UTCTime
  , eventSource :: Source
  , eventValue :: a
  }
  deriving (Eq, Show, Functor)

data Source = Source
  { sourceHost :: ByteString
  , sourceService :: ByteString
  }
  deriving (Eq, Ord, Show)
