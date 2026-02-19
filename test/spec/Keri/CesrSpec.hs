module Keri.CesrSpec (spec) where

import Data.ByteString qualified as BS
import Keri.Cesr
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "CESR roundtrip" $ do
        it "roundtrips arbitrary 32-byte Ed25519 keys" $
            property $
                forAll (BS.pack <$> vectorOf 32 arbitrary) $
                    \raw ->
                        let prim = Primitive Ed25519PubKey raw
                        in  decode (encode prim) === Right prim

        it "roundtrips arbitrary 32-byte digests" $
            property $
                forAll (BS.pack <$> vectorOf 32 arbitrary) $
                    \raw ->
                        let prim = Primitive Blake2bDigest raw
                        in  decode (encode prim) === Right prim

        it "roundtrips arbitrary 64-byte signatures" $
            property $
                forAll (BS.pack <$> vectorOf 64 arbitrary) $
                    \raw ->
                        let prim = Primitive Ed25519Sig raw
                        in  decode (encode prim) === Right prim

    describe "mkPrimitive" $ do
        it "rejects wrong size" $
            mkPrimitive Ed25519PubKey (BS.replicate 16 0)
                `shouldSatisfy` isLeft

        it "accepts correct size" $
            mkPrimitive Ed25519PubKey (BS.replicate 32 0)
                `shouldSatisfy` isRight
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False
