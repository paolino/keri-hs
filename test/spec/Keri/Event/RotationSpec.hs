module Keri.Event.RotationSpec (spec) where

import Data.Text qualified as T
import Keri.Event
import Keri.Event.Rotation
import Test.Hspec

spec :: Spec
spec = do
    describe "mkRotation" $ do
        it "creates a rotation event" $ do
            let cfg =
                    RotationConfig
                        { rcPrefix = "Eprefix"
                        , rcSequenceNumber = 1
                        , rcPriorDigest = "Eprior"
                        , rcKeys = ["Dkey1"]
                        , rcSigningThreshold = 1
                        , rcNextKeys = ["Enext1"]
                        , rcNextThreshold = 1
                        , rcConfig = []
                        , rcAnchors = []
                        }
                evt = mkRotation cfg
            eventType evt `shouldBe` Rot

        it "preserves prefix" $ do
            let cfg =
                    RotationConfig
                        { rcPrefix = "Eprefix"
                        , rcSequenceNumber = 1
                        , rcPriorDigest = "Eprior"
                        , rcKeys = ["Dkey1"]
                        , rcSigningThreshold = 1
                        , rcNextKeys = ["Enext1"]
                        , rcNextThreshold = 1
                        , rcConfig = []
                        , rcAnchors = []
                        }
                evt = mkRotation cfg
            eventPrefix evt `shouldBe` "Eprefix"

        it "SAID differs from prefix" $ do
            let cfg =
                    RotationConfig
                        { rcPrefix = "Eprefix"
                        , rcSequenceNumber = 1
                        , rcPriorDigest = "Eprior"
                        , rcKeys = ["Dkey1"]
                        , rcSigningThreshold = 1
                        , rcNextKeys = ["Enext1"]
                        , rcNextThreshold = 1
                        , rcConfig = []
                        , rcAnchors = []
                        }
                evt = mkRotation cfg
            eventDigest evt `shouldNotBe` "Eprefix"

        it "SAID starts with E" $ do
            let cfg =
                    RotationConfig
                        { rcPrefix = "Eprefix"
                        , rcSequenceNumber = 1
                        , rcPriorDigest = "Eprior"
                        , rcKeys = ["Dkey1"]
                        , rcSigningThreshold = 1
                        , rcNextKeys = ["Enext1"]
                        , rcNextThreshold = 1
                        , rcConfig = []
                        , rcAnchors = []
                        }
                evt = mkRotation cfg
            T.take 1 (eventDigest evt)
                `shouldBe` "E"
