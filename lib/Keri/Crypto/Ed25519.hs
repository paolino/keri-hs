module Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , sign
    , verify
    , publicKeyBytes
    , secretKeyBytes
    , signatureBytes
    , publicKeyFromBytes
    , secretKeyFromBytes
    , signatureFromBytes
    ) where

-- \|
-- Module      : Keri.Crypto.Ed25519
-- Description : Ed25519 key operations
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Ed25519 key generation, signing, and verification
-- using the @crypton@ library.

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)

-- | An Ed25519 key pair.
data KeyPair = KeyPair
    { secretKey :: Ed.SecretKey
    , publicKey :: Ed.PublicKey
    }

instance Show KeyPair where
    show _ = "KeyPair{..}"

-- | Generate a fresh random Ed25519 key pair.
generateKeyPair :: IO KeyPair
generateKeyPair = do
    sk <- Ed.generateSecretKey
    pure
        KeyPair
            { secretKey = sk
            , publicKey = Ed.toPublic sk
            }

-- | Sign a message with a key pair.
sign :: KeyPair -> ByteString -> ByteString
sign KeyPair{secretKey, publicKey} msg =
    BA.convert $
        Ed.sign secretKey publicKey msg

{- | Verify a signature against a public key and
message.
-}
verify
    :: Ed.PublicKey
    -> ByteString
    -> ByteString
    -> Bool
verify pk msg sig =
    case signatureFromBytes sig of
        Left _ -> False
        Right s -> Ed.verify pk msg s

-- | Extract raw bytes from a public key.
publicKeyBytes :: Ed.PublicKey -> ByteString
publicKeyBytes = BA.convert

-- | Extract raw bytes from a secret key.
secretKeyBytes :: Ed.SecretKey -> ByteString
secretKeyBytes = BA.convert

-- | Extract raw bytes from a signature.
signatureBytes :: Ed.Signature -> ByteString
signatureBytes = BA.convert

-- | Parse a public key from raw bytes.
publicKeyFromBytes
    :: ByteString -> Either String Ed.PublicKey
publicKeyFromBytes bs = case Ed.publicKey bs of
    CryptoPassed k -> Right k
    CryptoFailed e -> Left (show e)

-- | Parse a secret key from raw bytes.
secretKeyFromBytes
    :: ByteString -> Either String Ed.SecretKey
secretKeyFromBytes bs = case Ed.secretKey bs of
    CryptoPassed k -> Right k
    CryptoFailed e -> Left (show e)

-- | Parse a signature from raw bytes.
signatureFromBytes
    :: ByteString -> Either String Ed.Signature
signatureFromBytes bs = case Ed.signature bs of
    CryptoPassed s -> Right s
    CryptoFailed e -> Left (show e)
