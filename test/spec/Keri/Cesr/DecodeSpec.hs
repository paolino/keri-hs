module Keri.Cesr.DecodeSpec (spec) where

import Data.ByteString qualified as BS
import Keri.Cesr.Decode
import Keri.Cesr.DerivationCode
import Keri.Cesr.Encode qualified as Enc
import Keri.Cesr.Primitive
import Test.Hspec

spec :: Spec
spec = do
    describe "decode" $ do
        it "roundtrips Ed25519PubKey" $ do
            let raw = BS.replicate 32 42
                prim = Primitive Ed25519PubKey raw
                encoded = Enc.encode prim
            decode encoded `shouldBe` Right prim

        it "roundtrips Ed25519Sig" $ do
            let raw = BS.replicate 64 99
                prim = Primitive Ed25519Sig raw
                encoded = Enc.encode prim
            decode encoded `shouldBe` Right prim

        it "roundtrips Blake2bDigest" $ do
            let raw = BS.replicate 32 7
                prim = Primitive Blake2bDigest raw
                encoded = Enc.encode prim
            decode encoded `shouldBe` Right prim

        it "decodes keripy test vector" $ do
            let qb64 =
                    "DKxy2sgzfplyr-tgwIxS19f2OchFHtLwPWD3v4oYimBx"
                expected =
                    BS.pack
                        [ 0xac, 0x72, 0xda, 0xc8
                        , 0x33, 0x7e, 0x99, 0x72
                        , 0xaf, 0xeb, 0x60, 0xc0
                        , 0x8c, 0x52, 0xd7, 0xd7
                        , 0xf6, 0x39, 0xc8, 0x45
                        , 0x1e, 0xd2, 0xf0, 0x3d
                        , 0x60, 0xf7, 0xbf, 0x8a
                        , 0x18, 0x8a, 0x60, 0x71
                        ]
            case decode qb64 of
                Left err -> expectationFailure err
                Right prim -> do
                    code prim
                        `shouldBe` Ed25519PubKey
                    raw prim `shouldBe` expected

        it "rejects too-short input" $
            decode "D"
                `shouldSatisfy` isLeft

        it "rejects unknown codes" $
            decode "Zxxxxxxxxx"
                `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
