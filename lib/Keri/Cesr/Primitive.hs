module Keri.Cesr.Primitive
    ( Primitive (..)
    , mkPrimitive
    ) where

{- |
Module      : Keri.Cesr.Primitive
Description : Qualified cryptographic primitive
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

A 'Primitive' pairs a 'DerivationCode' with raw bytes,
representing a qualified cryptographic value (public key,
digest, or signature).
-}

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Keri.Cesr.DerivationCode
    ( DerivationCode
    , rawSize
    )

-- | A qualified CESR primitive: code plus raw bytes.
data Primitive = Primitive
    { code :: DerivationCode
    , raw :: ByteString
    }
    deriving stock (Show, Eq)

{- | Smart constructor that validates the raw bytes
length matches the derivation code.
-}
mkPrimitive
    :: DerivationCode
    -> ByteString
    -> Either String Primitive
mkPrimitive c bs
    | BS.length bs /= rawSize c =
        Left $
            "Expected "
                <> show (rawSize c)
                <> " bytes, got "
                <> show (BS.length bs)
    | otherwise = Right (Primitive c bs)
