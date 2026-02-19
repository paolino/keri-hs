module Keri.Event.InceptionSpec (spec) where

import Data.Text qualified as T
import Keri.Event
import Keri.Event.Inception
import Keri.Event.Version (parseVersionSize)
import Test.Hspec

spec :: Spec
spec = do
    describe "mkInception" $ do
        it "creates an inception event" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                evt = mkInception cfg
            eventType evt `shouldBe` Icp

        it "sets prefix equal to digest" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                evt = mkInception cfg
            eventPrefix evt
                `shouldBe` eventDigest evt

        it "sets sequence number to 0" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                evt = mkInception cfg
            eventSequenceNumber evt `shouldBe` 0

        it "SAID is not placeholder" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                evt = mkInception cfg
                said = eventDigest evt
            T.all (== '#') said `shouldBe` False

        it "SAID starts with E (digest code)" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                evt = mkInception cfg
            T.take 1 (eventDigest evt)
                `shouldBe` "E"

        it "version has correct size" $ do
            let cfg =
                    InceptionConfig
                        { keys = ["Dkey123"]
                        , signingThreshold = 1
                        , nextKeys = ["Edigest1"]
                        , nextThreshold = 1
                        , config = []
                        , anchors = []
                        }
                Inception d = mkInception cfg
                v = version (d :: InceptionData)
            parseVersionSize v
                `shouldSatisfy` maybe False (> 0)
