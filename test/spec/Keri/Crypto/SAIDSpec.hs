module Keri.Crypto.SAIDSpec (spec) where

import Data.Text (Text)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Digest (saidPlaceholder)
import Keri.Crypto.Ed25519 qualified as Ed
import Keri.Crypto.SAID (replaceDigest, verifySaid)
import Keri.Event
    ( Event (..)
    , InceptionData (..)
    , InteractionData (..)
    , eventDigest
    , eventPrefix
    )
import Keri.Event.Inception
import Keri.Event.Interaction
import Keri.Event.Rotation
import Keri.KeyState.PreRotation (commitKey)
import Test.Hspec

spec :: Spec
spec = describe "SAID" $ do
    describe "verifySaid" $ do
        it "succeeds for mkInception" $ do
            evt <- mkTestInception
            verifySaid evt `shouldBe` True

        it "succeeds for mkInteraction" $ do
            icp <- mkTestInception
            let ixn =
                    mkInteraction
                        InteractionConfig
                            { ixPrefix = eventPrefix icp
                            , ixSequenceNumber = 1
                            , ixPriorDigest =
                                eventDigest icp
                            , ixAnchors = []
                            }
            verifySaid ixn `shouldBe` True

        it "succeeds for mkRotation" $ do
            (icp, nextCesr) <- mkTestInceptionWithNext
            let rot =
                    mkRotation
                        RotationConfig
                            { rcPrefix = eventPrefix icp
                            , rcSequenceNumber = 1
                            , rcPriorDigest =
                                eventDigest icp
                            , rcKeys = [nextCesr]
                            , rcSigningThreshold = 1
                            , rcNextKeys = []
                            , rcNextThreshold = 0
                            , rcConfig = []
                            , rcAnchors = []
                            }
            verifySaid rot `shouldBe` True

        it "fails for tampered signingThreshold" $ do
            evt <- mkTestInception
            case evt of
                Inception InceptionData{..} ->
                    verifySaid
                        ( Inception
                            InceptionData
                                { signingThreshold = 99
                                , ..
                                }
                        )
                        `shouldBe` False
                _ -> expectationFailure "expected inception"

        it "fails for wrong digest field" $ do
            evt <- mkTestInception
            case evt of
                Inception InceptionData{..} ->
                    verifySaid
                        ( Inception
                            InceptionData
                                { digest = "wrong"
                                , ..
                                }
                        )
                        `shouldBe` False
                _ -> expectationFailure "expected inception"

    describe "replaceDigest" $ do
        it "replaces inception digest + prefix" $ do
            evt <- mkTestInception
            case replaceDigest evt of
                Inception InceptionData{digest, prefix} ->
                    do
                        digest `shouldBe` saidPlaceholder
                        prefix `shouldBe` saidPlaceholder
                _ -> expectationFailure "expected inception"

        it "replaces interaction digest only" $ do
            icp <- mkTestInception
            let ixn =
                    mkInteraction
                        InteractionConfig
                            { ixPrefix = eventPrefix icp
                            , ixSequenceNumber = 1
                            , ixPriorDigest =
                                eventDigest icp
                            , ixAnchors = []
                            }
            case replaceDigest ixn of
                Interaction
                    InteractionData{digest, prefix} -> do
                        digest `shouldBe` saidPlaceholder
                        prefix
                            `shouldNotBe` saidPlaceholder
                _ ->
                    expectationFailure
                        "expected interaction"

mkTestInception :: IO Event
mkTestInception = do
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodeKey kp
        nextPubCesr = encodeKey nextKp
    nextCommit <-
        either fail pure $ commitKey nextPubCesr
    pure $
        mkInception
            InceptionConfig
                { icKeys = [pubCesr]
                , icSigningThreshold = 1
                , icNextKeys = [nextCommit]
                , icNextThreshold = 1
                , icConfig = []
                , icAnchors = []
                }

mkTestInceptionWithNext :: IO (Event, Text)
mkTestInceptionWithNext = do
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodeKey kp
        nextPubCesr = encodeKey nextKp
    nextCommit <-
        either fail pure $ commitKey nextPubCesr
    let evt =
            mkInception
                InceptionConfig
                    { icKeys = [pubCesr]
                    , icSigningThreshold = 1
                    , icNextKeys = [nextCommit]
                    , icNextThreshold = 1
                    , icConfig = []
                    , icAnchors = []
                    }
    pure (evt, nextPubCesr)

encodeKey :: Ed.KeyPair -> Text
encodeKey kp =
    Cesr.encode $
        Primitive
            Ed25519PubKey
            (Ed.publicKeyBytes (Ed.publicKey kp))
