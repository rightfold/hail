module Hail.Protocol
  ( -- * Packet parsing
    getPacket
  , getMagic
  , getVersion

    -- * Event parsing
  , getEvent
  , getEventTime
  , getEventSource
  , getEventValue

    -- * Low-level parsing
  , getString
  ) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, getBytes, getInt64be, getWord16be, getWord32be)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word16)
import Hail.Event (Event(..), Source(..))

--------------------------------------------------------------------------------

getPacket :: Get a -> Get a
getPacket = (getMagic *> (guard . (0 ==) =<< getVersion) *>)

getMagic :: Get ()
getMagic = guard . (0xBABE ==) =<< getWord16be

getVersion :: Get Word16
getVersion = getWord16be

--------------------------------------------------------------------------------

getEvent :: Get (Event ByteString)
getEvent = Event <$> getEventTime <*> getEventSource <*> getEventValue

getEventTime :: Get UTCTime
getEventTime = posixSecondsToUTCTime . fromIntegral <$> getInt64be

getEventSource :: Get Source
getEventSource = Source <$> getString <*> getString

getEventValue :: Get ByteString
getEventValue = getString

--------------------------------------------------------------------------------

getString :: Get ByteString
getString = getWord32be >>= getBytes . fromIntegral
