module Keri.KelSpec (spec) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed
import Keri.Event
import Keri.Event.Inception
import Keri.Event.Interaction
import Keri.Event.Rotation
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel
import Keri.Kel.Append qualified as Kel
import Keri.Kel.Replay qualified as Replay
import Keri.KeyState qualified as KS
import Keri.KeyState.PreRotation (commitKey)
import Test.Hspec

spec :: Spec
spec = do
    describe "append" $ do
        it "appends inception to empty KEL" $ do
            (se, _, _) <- mkSignedInception
            case Kel.append emptyKel se of
                Left err ->
                    expectationFailure err
                Right (Kel events) ->
                    length events `shouldBe` 1

        it "rejects non-inception as first event" $
            do
                let bad =
                        SignedEvent
                            { event =
                                Interaction
                                    InteractionData
                                        { version = "v"
                                        , digest = "d"
                                        , prefix = "i"
                                        , sequenceNumber = 0
                                        , priorDigest = "p"
                                        , anchors = []
                                        }
                            , signatures = []
                            }
                Kel.append emptyKel bad
                    `shouldSatisfy` isLeft

    describe "replay" $ do
        it "replays single inception" $ do
            (se, _, _) <- mkSignedInception
            case Kel.append emptyKel se of
                Left err ->
                    expectationFailure err
                Right kel ->
                    case Replay.replay kel of
                        Left err ->
                            expectationFailure err
                        Right ks ->
                            KS.stateSequenceNumber ks
                                `shouldBe` 0

        it "replays inception + interaction" $ do
            result <- mkKelWithInteraction
            case result of
                Left err ->
                    expectationFailure err
                Right kel ->
                    case Replay.replay kel of
                        Left err ->
                            expectationFailure err
                        Right ks ->
                            KS.stateSequenceNumber ks
                                `shouldBe` 1

        it "rejects empty KEL" $
            Replay.replay emptyKel
                `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False

mkSignedInception
    :: IO (SignedEvent, Ed.KeyPair, Ed.KeyPair)
mkSignedInception = do
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodeKey kp
        nextPubCesr = encodeKey nextKp
    nextCommit <-
        either fail pure $
            commitKey nextPubCesr
    let cfg =
            InceptionConfig
                { keys = [pubCesr]
                , signingThreshold = 1
                , nextKeys = [nextCommit]
                , nextThreshold = 1
                , config = []
                , anchors = []
                }
        evt = mkInception cfg
        msgBytes = serializeEvent evt
        sig = Ed.sign kp msgBytes
        sigCesr = encodeSig sig
        se =
            SignedEvent
                { event = evt
                , signatures = [(0, sigCesr)]
                }
    pure (se, kp, nextKp)

mkKelWithInteraction :: IO (Either String Kel)
mkKelWithInteraction = do
    (se, kp, _) <- mkSignedInception
    case Kel.append emptyKel se of
        Left err -> pure (Left err)
        Right kel -> do
            let icp = event se
                ixnCfg =
                    InteractionConfig
                        { prefix = eventPrefix icp
                        , sequenceNumber = 1
                        , priorDigest =
                            eventDigest icp
                        , anchors = []
                        }
                ixnEvt = mkInteraction ixnCfg
                ixnBytes = serializeEvent ixnEvt
                ixnSig = Ed.sign kp ixnBytes
                ixnSigCesr = encodeSig ixnSig
                ixnSe =
                    SignedEvent
                        { event = ixnEvt
                        , signatures =
                            [(0, ixnSigCesr)]
                        }
            pure (Kel.append kel ixnSe)

encodeKey :: Ed.KeyPair -> Text
encodeKey kp =
    Cesr.encode $
        Primitive
            Ed25519PubKey
            (Ed.publicKeyBytes (Ed.publicKey kp))

encodeSig :: ByteString -> Text
encodeSig sig =
    Cesr.encode $
        Primitive Ed25519Sig sig
