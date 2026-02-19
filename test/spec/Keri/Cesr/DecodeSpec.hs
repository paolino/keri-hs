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
                        [ 0xac
                        , 0x72
                        , 0xda
                        , 0xc8
                        , 0x33
                        , 0x7e
                        , 0x99
                        , 0x72
                        , 0xaf
                        , 0xeb
                        , 0x60
                        , 0xc0
                        , 0x8c
                        , 0x52
                        , 0xd7
                        , 0xd7
                        , 0xf6
                        , 0x39
                        , 0xc8
                        , 0x45
                        , 0x1e
                        , 0xd2
                        , 0xf0
                        , 0x3d
                        , 0x60
                        , 0xf7
                        , 0xbf
                        , 0x8a
                        , 0x18
                        , 0x8a
                        , 0x60
                        , 0x71
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

    describe "cesr-test-vectors" $ do
        -- Vectors from WebOfTrust/cesr-test-vectors
        let raw32 =
                BS.pack
                    [ 0x5f
                    , 0x9c
                    , 0xd8
                    , 0x45
                    , 0xa8
                    , 0x9d
                    , 0xab
                    , 0x7b
                    , 0xf0
                    , 0xde
                    , 0xe3
                    , 0x3d
                    , 0xa6
                    , 0xf0
                    , 0xa5
                    , 0x96
                    , 0xe1
                    , 0x89
                    , 0x05
                    , 0xcf
                    , 0xc2
                    , 0x85
                    , 0x29
                    , 0x15
                    , 0x5f
                    , 0xa8
                    , 0xd2
                    , 0x94
                    , 0x31
                    , 0x9e
                    , 0xdf
                    , 0xe0
                    ]

        it "decodes code D" $ do
            let qb64 =
                    "DF-c2EWonat78N7jPabwpZbhiQXPwoUpFV-o0pQxnt_g"
            case decode qb64 of
                Left err -> expectationFailure err
                Right prim -> do
                    code prim
                        `shouldBe` Ed25519PubKey
                    raw prim `shouldBe` raw32

        it "decodes code E" $ do
            let qb64 =
                    "EF-c2EWonat78N7jPabwpZbhiQXPwoUpFV-o0pQxnt_g"
            case decode qb64 of
                Left err -> expectationFailure err
                Right prim -> do
                    code prim
                        `shouldBe` Blake2bDigest
                    raw prim `shouldBe` raw32

        it "decodes code 0B" $ do
            let qb64 =
                    "0BA-meOtrZqh25qrCF30C_TuNDz5Et2F3t2rLbJ98TqXCelY8I4ZpkZQy7iD93SRxZTATQTFQF99U0XNA_JPgq0N"
                raw64 =
                    BS.pack
                        [ 0x3e
                        , 0x99
                        , 0xe3
                        , 0xad
                        , 0xad
                        , 0x9a
                        , 0xa1
                        , 0xdb
                        , 0x9a
                        , 0xab
                        , 0x08
                        , 0x5d
                        , 0xf4
                        , 0x0b
                        , 0xf4
                        , 0xee
                        , 0x34
                        , 0x3c
                        , 0xf9
                        , 0x12
                        , 0xdd
                        , 0x85
                        , 0xde
                        , 0xdd
                        , 0xab
                        , 0x2d
                        , 0xb2
                        , 0x7d
                        , 0xf1
                        , 0x3a
                        , 0x97
                        , 0x09
                        , 0xe9
                        , 0x58
                        , 0xf0
                        , 0x8e
                        , 0x19
                        , 0xa6
                        , 0x46
                        , 0x50
                        , 0xcb
                        , 0xb8
                        , 0x83
                        , 0xf7
                        , 0x74
                        , 0x91
                        , 0xc5
                        , 0x94
                        , 0xc0
                        , 0x4d
                        , 0x04
                        , 0xc5
                        , 0x40
                        , 0x5f
                        , 0x7d
                        , 0x53
                        , 0x45
                        , 0xcd
                        , 0x03
                        , 0xf2
                        , 0x4f
                        , 0x82
                        , 0xad
                        , 0x0d
                        ]
            case decode qb64 of
                Left err -> expectationFailure err
                Right prim -> do
                    code prim
                        `shouldBe` Ed25519Sig
                    raw prim `shouldBe` raw64
  where
    isLeft (Left _) = True
    isLeft _ = False
