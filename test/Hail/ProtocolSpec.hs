{-# LANGUAGE OverloadedStrings #-}
module Hail.ProtocolSpec
  ( spec
  ) where

import Data.ByteString (pack)
import Data.Serialize.Get (Get, runGet)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word8)
import Hail.Event (Event(..), Source(..))
import Hail.Protocol (getEvent, getPacket)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ((==>), property)

fromRight :: Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right x) = Just x

run :: Get a -> [Word8] -> Maybe a
run g b = fromRight (runGet g (pack b))

spec :: Spec
spec = do
  describe "getPacket" $ do
    it "succeeds" $
      run (getPacket (pure ())) [0xBA, 0xBE, 0x00, 0x00]
        `shouldBe` Just ()
    it "fails on incorrect magic numbers" . property $
      \a b -> (a, b) /= (0xBA, 0xBE) ==>
        run (getPacket (pure ())) [a, b, 0x00, 0x00]
          `shouldBe` Nothing
    it "fails on incorrect version numbers" . property $
      \a b -> (a, b) /= (0x00, 0x00) ==>
        run (getPacket (pure ())) [0xBA, 0xBE, a, b]
          `shouldBe` Nothing
  describe "getEvent" $ do
    it "succeeds" . property $
      run getEvent [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01
                   , 0x00, 0x00, 0x00, 0x01, 0x0A
                   , 0x00, 0x00, 0x00, 0x02, 0x0B, 0x0C
                   , 0x00, 0x00, 0x00, 0x03, 0x0D, 0x0E, 0x0F
                   ]
        `shouldBe` Just (Event (posixSecondsToUTCTime 1)
                               (Source "\x0A" "\x0B\x0C")
                               "\x0D\x0E\x0F")
    it "fails on incomplete source hosts" . property $
      \a b c d -> d > 0x04 ==>
        run getEvent [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                     , a,    b,    c,    d,    0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     ]
          `shouldBe` Nothing
    it "fails on incomplete source services" . property $
      \a b c d -> d > 0x04 ==>
        run getEvent [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     , a,    b,    c,    d,    0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     ]
          `shouldBe` Nothing
    it "fails on incomplete values" . property $
      \a b c d -> d > 0x04 ==>
        run getEvent [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     , 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00
                     , a,    b,    c,    d,    0x00, 0x00, 0x00, 0x00
                     ]
          `shouldBe` Nothing
