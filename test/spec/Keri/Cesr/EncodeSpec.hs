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
                            [ 0xac, 0x72, 0xda, 0xc8
                            , 0x33, 0x7e, 0x99, 0x72
                            , 0xaf, 0xeb, 0x60, 0xc0
                            , 0x8c, 0x52, 0xd7, 0xd7
                            , 0xf6, 0x39, 0xc8, 0x45
                            , 0x1e, 0xd2, 0xf0, 0x3d
                            , 0x60, 0xf7, 0xbf, 0x8a
                            , 0x18, 0x8a, 0x60, 0x71
                            ]
                    prim =
                        Primitive Ed25519PubKey verkey
                    -- keripy Verfer with code=Ed25519
                    -- gives qb64 with prefix 'D'
                    expected =
                        "DKxy2sgzfplyr-tgwIxS19f2OchFHtLwPWD3v4oYimBx"
                encode prim `shouldBe` expected
