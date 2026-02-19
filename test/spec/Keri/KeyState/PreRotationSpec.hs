module Keri.KeyState.PreRotationSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.KeyState.PreRotation
import Test.Hspec

spec :: Spec
spec = do
    describe "commitKey" $ do
        it "produces a digest commitment" $ do
            let key =
                    Cesr.encode $
                        Primitive
                            Ed25519PubKey
                            (BS.replicate 32 1)
            case commitKey key of
                Left err ->
                    expectationFailure err
                Right commitment ->
                    T.take 1 commitment
                        `shouldBe` "E"

        it "is deterministic" $ do
            let key =
                    Cesr.encode $
                        Primitive
                            Ed25519PubKey
                            (BS.replicate 32 42)
            case (commitKey key, commitKey key) of
                (Right c1, Right c2) ->
                    c1 `shouldBe` c2
                _ ->
                    expectationFailure
                        "commitKey failed"

    describe "verifyCommitment" $ do
        it "verifies matching key and commitment" $
            do
                let key =
                        Cesr.encode $
                            Primitive
                                Ed25519PubKey
                                (BS.replicate 32 5)
                case commitKey key of
                    Left err ->
                        expectationFailure err
                    Right commitment -> do
                        result <-
                            either
                                (fail . show)
                                pure
                                $ verifyCommitment
                                    key
                                    commitment
                        result `shouldBe` True

        it "rejects wrong key" $ do
            let key1 =
                    Cesr.encode $
                        Primitive
                            Ed25519PubKey
                            (BS.replicate 32 1)
                key2 =
                    Cesr.encode $
                        Primitive
                            Ed25519PubKey
                            (BS.replicate 32 2)
            case commitKey key1 of
                Left err ->
                    expectationFailure err
                Right commitment -> do
                    result <-
                        either
                            (fail . show)
                            pure
                            $ verifyCommitment
                                key2
                                commitment
                    result `shouldBe` False
