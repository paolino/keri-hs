module Keri.Crypto.DigestSpec (spec) where

import Data.Text qualified as T
import Keri.Crypto.Digest
import Test.Hspec

spec :: Spec
spec = do
    describe "saidPlaceholder" $ do
        it "is 44 characters" $
            T.length saidPlaceholder `shouldBe` 44

        it "is all # characters" $
            T.all (== '#') saidPlaceholder
                `shouldBe` True

    describe "computeSaid" $ do
        it "returns 44-char CESR text" $ do
            let said = computeSaid "test bytes"
            T.length said `shouldBe` 44

        it "starts with F (digest code)" $ do
            let said = computeSaid "test bytes"
            T.take 1 said `shouldBe` "F"

        it "is deterministic" $ do
            let s1 = computeSaid "same input"
                s2 = computeSaid "same input"
            s1 `shouldBe` s2

        it "differs for different inputs" $ do
            let s1 = computeSaid "input a"
                s2 = computeSaid "input b"
            s1 `shouldNotBe` s2
