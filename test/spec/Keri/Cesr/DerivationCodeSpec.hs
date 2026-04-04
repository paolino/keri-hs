module Keri.Cesr.DerivationCodeSpec (spec) where

import Keri.Cesr.DerivationCode
import Test.Hspec

spec :: Spec
spec = do
    describe "codeText" $ do
        it "Ed25519PubKey is D" $
            codeText Ed25519PubKey `shouldBe` "D"
        it "Blake2bDigest is F" $
            codeText Blake2bDigest `shouldBe` "F"
        it "Ed25519Sig is 0B" $
            codeText Ed25519Sig `shouldBe` "0B"

    describe "rawSize" $ do
        it "Ed25519PubKey is 32 bytes" $
            rawSize Ed25519PubKey `shouldBe` 32
        it "Blake2bDigest is 32 bytes" $
            rawSize Blake2bDigest `shouldBe` 32
        it "Ed25519Sig is 64 bytes" $
            rawSize Ed25519Sig `shouldBe` 64

    describe "codeLength" $ do
        it "Ed25519PubKey is 1 char" $
            codeLength Ed25519PubKey `shouldBe` 1
        it "Blake2bDigest is 1 char" $
            codeLength Blake2bDigest `shouldBe` 1
        it "Ed25519Sig is 2 chars" $
            codeLength Ed25519Sig `shouldBe` 2

    describe "totalLength" $ do
        it "Ed25519PubKey is 44 chars" $
            totalLength Ed25519PubKey `shouldBe` 44
        it "Blake2bDigest is 44 chars" $
            totalLength Blake2bDigest `shouldBe` 44
        it "Ed25519Sig is 88 chars" $
            totalLength Ed25519Sig `shouldBe` 88

    describe "identifyCode" $ do
        it "identifies D as Ed25519PubKey" $
            identifyCode "Dxyz..."
                `shouldBe` Right Ed25519PubKey
        it "identifies F as Blake2bDigest" $
            identifyCode "Fxyz..."
                `shouldBe` Right Blake2bDigest
        it "identifies 0B as Ed25519Sig" $
            identifyCode "0Bxyz..."
                `shouldBe` Right Ed25519Sig
        it "rejects unknown codes" $
            identifyCode "Zxyz..."
                `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
