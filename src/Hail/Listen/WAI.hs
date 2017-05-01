{-# LANGUAGE OverloadedStrings #-}
module Hail.Listen.WAI
  ( Handler
  , Error(..)
  , application
  ) where

import Control.Monad ((<=<), (>=>), forever)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Serialize (runGetLazy)
import Hail.Event (Event)
import Hail.Protocol (getEvent, getPacket)
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (DataMessage(..), ServerApp, acceptRequest, defaultConnectionOptions, receiveDataMessage)

import qualified Data.ByteString.Lazy as ByteString.Lazy

type Handler = Either Error (Event ByteString) -> IO ()

data Error
  = TextPacket ByteString.Lazy.ByteString
  | ParseError String

application :: Handler -> Application
application h = websocketsOr defaultConnectionOptions (serverApp h) notWSReq

serverApp :: Handler -> ServerApp
serverApp h = acceptRequest >=> forever . (process <=< receiveDataMessage)
  where process (Text d) = h . Left . TextPacket $ d
        process (Binary d) = h . first ParseError . runGetLazy (getPacket getEvent) $ d

notWSReq :: Application
notWSReq _ res = res $ responseLBS status400 [] "Not a WebSocket request."
