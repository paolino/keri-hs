module Keri.Crypto.Ed25519Spec (spec) where

import Data.ByteString qualified as BS
import Keri.Crypto.Ed25519
import Test.Hspec

spec :: Spec
spec = do
    describe "generateKeyPair" $ do
        it "generates a key pair" $ do
            kp <- generateKeyPair
            BS.length (publicKeyBytes (publicKey kp))
                `shouldBe` 32

        it "generates distinct key pairs" $ do
            kp1 <- generateKeyPair
            kp2 <- generateKeyPair
            publicKeyBytes (publicKey kp1)
                `shouldNotBe` publicKeyBytes
                    (publicKey kp2)

    describe "sign and verify" $ do
        it "verifies a valid signature" $ do
            kp <- generateKeyPair
            let msg = "hello keri"
                sig = sign kp msg
            verify (publicKey kp) msg sig
                `shouldBe` True

        it "rejects wrong message" $ do
            kp <- generateKeyPair
            let sig = sign kp "hello"
            verify (publicKey kp) "wrong" sig
                `shouldBe` False

        it "rejects wrong key" $ do
            kp1 <- generateKeyPair
            kp2 <- generateKeyPair
            let sig = sign kp1 "hello"
            verify (publicKey kp2) "hello" sig
                `shouldBe` False

    describe "publicKeyFromBytes" $ do
        it "roundtrips" $ do
            kp <- generateKeyPair
            let bs = publicKeyBytes (publicKey kp)
            publicKeyFromBytes bs
                `shouldSatisfy` isRight

        it "rejects wrong size" $
            publicKeyFromBytes (BS.replicate 16 0)
                `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False
