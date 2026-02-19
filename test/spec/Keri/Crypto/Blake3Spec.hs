module Keri.Crypto.Blake3Spec (spec) where

import Data.ByteString qualified as BS
import Keri.Crypto.Blake3
import Test.Hspec

spec :: Spec
spec = do
    describe "hash" $ do
        it "produces 32 bytes" $ do
            let h = hash "hello"
            BS.length h `shouldBe` 32

        it "is deterministic" $ do
            let h1 = hash "test input"
                h2 = hash "test input"
            h1 `shouldBe` h2

        it "differs for different inputs" $ do
            let h1 = hash "input a"
                h2 = hash "input b"
            h1 `shouldNotBe` h2

        it "hash size is 32" $
            hashSize `shouldBe` 32
