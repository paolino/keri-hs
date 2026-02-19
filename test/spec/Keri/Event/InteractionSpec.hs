module Keri.Event.InteractionSpec (spec) where

import Data.Text qualified as T
import Keri.Event
import Keri.Event.Interaction
import Test.Hspec

spec :: Spec
spec = do
    describe "mkInteraction" $ do
        it "creates an interaction event" $ do
            let cfg =
                    InteractionConfig
                        { prefix = "Eprefix"
                        , sequenceNumber = 2
                        , priorDigest = "Eprior"
                        , anchors = []
                        }
                evt = mkInteraction cfg
            eventType evt `shouldBe` Ixn

        it "preserves prefix" $ do
            let cfg =
                    InteractionConfig
                        { prefix = "Eprefix"
                        , sequenceNumber = 2
                        , priorDigest = "Eprior"
                        , anchors = []
                        }
                evt = mkInteraction cfg
            eventPrefix evt `shouldBe` "Eprefix"

        it "SAID starts with E" $ do
            let cfg =
                    InteractionConfig
                        { prefix = "Eprefix"
                        , sequenceNumber = 2
                        , priorDigest = "Eprior"
                        , anchors = []
                        }
                evt = mkInteraction cfg
            T.take 1 (eventDigest evt)
                `shouldBe` "E"
