module Keri.KeyStateSpec (spec) where

import Data.Text (Text)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed
import Keri.Event
import Keri.Event.Inception
import Keri.Event.Rotation
import Keri.KeyState
import Keri.KeyState.PreRotation (commitKey)
import Test.Hspec

spec :: Spec
spec = do
    describe "initialState" $ do
        it "sets prefix from inception" $ do
            (_, evt) <- mkTestInception
            case evt of
                Inception d@InceptionData{prefix} -> do
                    let ks = initialState d
                    statePrefix ks
                        `shouldBe` prefix
                _ ->
                    expectationFailure "not inception"

        it "sets sequence number to 0" $ do
            (_, evt) <- mkTestInception
            case evt of
                Inception d ->
                    stateSequenceNumber
                        (initialState d)
                        `shouldBe` 0
                _ ->
                    expectationFailure "not inception"

    describe "applyEvent" $ do
        it "rejects inception on existing state" $
            do
                (_, evt) <- mkTestInception
                case evt of
                    Inception d -> do
                        let ks = initialState d
                        applyEvent ks evt
                            `shouldSatisfy` isLeft
                    _ ->
                        expectationFailure
                            "not inception"

        it "applies rotation with valid pre-rotation" $
            do
                (kp, icp) <- mkTestInception
                case icp of
                    Inception d -> do
                        let ks = initialState d
                        -- The next keys from inception
                        -- match what we committed
                        rotEvt <-
                            mkTestRotation kp ks
                        case applyEvent ks rotEvt of
                            Left err ->
                                expectationFailure err
                            Right ks' ->
                                stateSequenceNumber ks'
                                    `shouldBe` 1
                    _ ->
                        expectationFailure
                            "not inception"

        it "rejects wrong sequence number" $ do
            (_, evt) <- mkTestInception
            case evt of
                Inception d -> do
                    let ks = initialState d
                        bad =
                            Interaction
                                InteractionData
                                    { version =
                                        "KERI10JSON000000_"
                                    , digest = "d"
                                    , prefix =
                                        statePrefix ks
                                    , sequenceNumber = 5
                                    , priorDigest =
                                        stateLastDigest ks
                                    , anchors = []
                                    }
                    applyEvent ks bad
                        `shouldSatisfy` isLeft
                _ ->
                    expectationFailure "not inception"
  where
    isLeft (Left _) = True
    isLeft _ = False

mkTestInception :: IO (Ed.KeyPair, Event)
mkTestInception = do
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodeKey kp
        nextPubCesr = encodeKey nextKp
    nextCommit <-
        either fail pure $
            commitKey nextPubCesr
    let cfg =
            InceptionConfig
                { icKeys = [pubCesr]
                , icSigningThreshold = 1
                , icNextKeys = [nextCommit]
                , icNextThreshold = 1
                , icConfig = []
                , icAnchors = []
                }
    pure (nextKp, mkInception cfg)

mkTestRotation
    :: Ed.KeyPair -> KeyState -> IO Event
mkTestRotation nextKp ks = do
    newNextKp <- Ed.generateKeyPair
    let newNextCesr = encodeKey newNextKp
    newNextCommit <-
        either fail pure $
            commitKey newNextCesr
    let cfg =
            RotationConfig
                { rcPrefix = statePrefix ks
                , rcSequenceNumber =
                    stateSequenceNumber ks + 1
                , rcPriorDigest =
                    stateLastDigest ks
                , rcKeys = [encodeKey nextKp]
                , rcSigningThreshold = 1
                , rcNextKeys = [newNextCommit]
                , rcNextThreshold = 1
                , rcConfig = []
                , rcAnchors = []
                }
    pure (mkRotation cfg)

encodeKey :: Ed.KeyPair -> Text
encodeKey kp =
    Cesr.encode $
        Primitive
            Ed25519PubKey
            (Ed.publicKeyBytes (Ed.publicKey kp))
