module Keri.Event.SerializeSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Keri.Event
import Keri.Event.Serialize
import Test.Hspec

spec :: Spec
spec = do
    describe "serializeEvent" $ do
        it "produces valid JSON for inception" $ do
            let d =
                    InceptionData
                        { version = "KERI10JSON000000_"
                        , digest = "placeholder"
                        , prefix = "placeholder"
                        , sequenceNumber = 0
                        , signingThreshold = 1
                        , keys = ["Dkey1"]
                        , nextThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , witnessThreshold = 0
                        , witnesses = []
                        , config = []
                        , anchors = []
                        }
                bs = serializeEvent (Inception d)
            -- Must start with {
            BS.take 1 bs `shouldBe` "{"
            -- Must contain "t":"icp"
            bs `shouldSatisfy` containsStr "\"t\":\"icp\""

        it "has deterministic field order" $ do
            let d =
                    InceptionData
                        { version = "KERI10JSON000000_"
                        , digest = "d"
                        , prefix = "i"
                        , sequenceNumber = 0
                        , signingThreshold = 1
                        , keys = []
                        , nextThreshold = 1
                        , nextKeys = []
                        , witnessThreshold = 0
                        , witnesses = []
                        , config = []
                        , anchors = []
                        }
                bs = serializeEvent (Inception d)
            -- v must come before t
            fieldPos "v" bs
                `shouldSatisfy` (< fieldPos "t" bs)
            -- t must come before d
            fieldPos "t" bs
                `shouldSatisfy` (< fieldPos "d" bs)

        it "serializes interaction" $ do
            let d =
                    InteractionData
                        { version = "KERI10JSON000000_"
                        , digest = "d"
                        , prefix = "i"
                        , sequenceNumber = 1
                        , priorDigest = "p"
                        , anchors = []
                        }
                bs = serializeEvent (Interaction d)
            bs `shouldSatisfy` containsStr "\"t\":\"ixn\""

        it "serializes receipt" $ do
            let d =
                    ReceiptData
                        { version = "KERI10JSON000000_"
                        , digest = "d"
                        , prefix = "i"
                        , sequenceNumber = 0
                        }
                bs = serializeEvent (Receipt d)
            bs `shouldSatisfy` containsStr "\"t\":\"rct\""

containsStr :: BS.ByteString -> BS.ByteString -> Bool
containsStr needle haystack =
    not $ BS.null $ snd $ BS.breakSubstring needle haystack

fieldPos :: BS.ByteString -> BS.ByteString -> Int
fieldPos field bs =
    let key = "\"" <> field <> "\""
        (before, _) = BS.breakSubstring key bs
    in BS.length before
