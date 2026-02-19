module Keri.KeyState.Verify
    ( verifySignatures
    , IndexedSignature
    ) where

{- |
Module      : Keri.KeyState.Verify
Description : Signature and threshold verification
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Verifies indexed signatures against a set of public
keys and a signing threshold.
-}

import Data.ByteString (ByteString)
import Data.Text (Text)
import Keri.Cesr.Decode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed

{- | An indexed signature: the index into the key
list and the CESR-encoded signature.
-}
type IndexedSignature = (Int, Text)

{- | Verify that enough valid signatures meet the
threshold. Each signature's index maps to a position
in the key list.
-}
verifySignatures
    :: [Text]
    -- ^ CESR-encoded public keys
    -> Int
    -- ^ Signing threshold
    -> ByteString
    -- ^ Message (serialized event bytes)
    -> [IndexedSignature]
    -- ^ Indexed signatures
    -> Either String Bool
verifySignatures pubKeys threshold msg sigs = do
    validCount <- countValid pubKeys msg sigs
    pure (validCount >= threshold)

countValid
    :: [Text]
    -> ByteString
    -> [IndexedSignature]
    -> Either String Int
countValid pubKeys msg = go 0
  where
    go !acc [] = Right acc
    go !acc ((idx, cesrSig) : rest) = do
        if idx < 0 || idx >= length pubKeys
            then
                Left $
                    "Signature index out of range: "
                        <> show idx
            else do
                keyPrim <-
                    Cesr.decode (pubKeys !! idx)
                sigPrim <- Cesr.decode cesrSig
                pk <-
                    Ed.publicKeyFromBytes (raw keyPrim)
                let valid =
                        Ed.verify pk msg (raw sigPrim)
                go
                    (if valid then acc + 1 else acc)
                    rest
