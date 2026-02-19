module Keri.Cesr.EncodeSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Keri.Cesr.DerivationCode
import Keri.Cesr.Encode
import Keri.Cesr.Primitive
import Test.Hspec

spec :: Spec
spec = do
    describe "encode" $ do
        it "encodes a 32-byte key to 44 chars" $ do
            let raw = BS.replicate 32 0
                prim = Primitive Ed25519PubKey raw
            T.length (encode prim)
                `shouldBe` 44

        it "starts with code prefix D" $ do
            let raw = BS.replicate 32 0
                prim = Primitive Ed25519PubKey raw
            T.take 1 (encode prim)
                `shouldBe` "D"

        it "encodes a 64-byte sig to 88 chars" $ do
            let raw = BS.replicate 64 0
                prim = Primitive Ed25519Sig raw
            T.length (encode prim)
                `shouldBe` 88

        it "starts with code prefix 0B" $ do
            let raw = BS.replicate 64 0
                prim = Primitive Ed25519Sig raw
            T.take 2 (encode prim)
                `shouldBe` "0B"

        it "encodes a digest with code E" $ do
            let raw = BS.replicate 32 0
                prim = Primitive Blake2bDigest raw
            T.take 1 (encode prim)
                `shouldBe` "E"

        it "matches keripy test vector for Ed25519" $
            do
                -- From keripy: verkey bytes -> qb64
                let verkey :: ByteString
                    verkey =
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
                    prim =
                        Primitive Ed25519PubKey verkey
                    expected =
                        "DKxy2sgzfplyr-tgwIxS19f2OchFHtLwPWD3v4oYimBx"
                encode prim `shouldBe` expected

    describe "cesr-test-vectors" $ do
        -- Vectors from WebOfTrust/cesr-test-vectors
        -- Same 32-byte raw value encoded with D/E
        let raw32 :: ByteString
            raw32 =
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

        it "code D (Ed25519 transferable)" $
            encode (Primitive Ed25519PubKey raw32)
                `shouldBe` "DF-c2EWonat78N7jPabwpZbhiQXPwoUpFV-o0pQxnt_g"

        it "code E (Blake3/2b digest)" $
            encode (Primitive Blake2bDigest raw32)
                `shouldBe` "EF-c2EWonat78N7jPabwpZbhiQXPwoUpFV-o0pQxnt_g"

        it "code 0B (Ed25519 signature)" $ do
            let raw64 :: ByteString
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
            encode (Primitive Ed25519Sig raw64)
                `shouldBe` "0BA-meOtrZqh25qrCF30C_TuNDz5Et2F3t2rLbJ98TqXCelY8I4ZpkZQy7iD93SRxZTATQTFQF99U0XNA_JPgq0N"
