module Hail.ProtocolSpec
  ( spec
  ) where

import Data.ByteString (pack)
import Data.Serialize.Get (runGet)
import Hail.Protocol (getPacket)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ((==>), property)

fromRight :: Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right x) = Just x

spec :: Spec
spec =
  describe "getPacket" $ do
    it "succeeds" $
      fromRight (runGet (getPacket (pure ())) (pack [0xBA, 0xBE, 0x00, 0x00]))
        `shouldBe` Just ()
    it "fails on incorrect magic numbers" . property $
      \a b -> (a, b) /= (0xBA, 0xBE) ==>
        fromRight (runGet (getPacket (pure ())) (pack [a, b, 0x00, 0x00]))
          `shouldBe` Nothing
    it "fails on incorrect version numbers" . property $
      \a b -> (a, b) /= (0x00, 0x00) ==>
        fromRight (runGet (getPacket (pure ())) (pack [0xBA, 0xBE, a, b]))
          `shouldBe` Nothing
