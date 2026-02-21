module Keri.Event.Serialize
    ( serializeEvent
    , encodeEvent
    ) where

-- \|
-- Module      : Keri.Event.Serialize
-- Description : Canonical JSON serialization
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Serializes KERI events to deterministic JSON with
-- protocol-defined field ordering using aeson's
-- 'Encoding' builder.

import Data.Aeson.Encoding
    ( Encoding
    , encodingToLazyByteString
    , list
    , pair
    , pairs
    , text
    , value
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Keri.Event
    ( Event (..)
    , InceptionData (..)
    , InteractionData (..)
    , ReceiptData (..)
    , RotationData (..)
    )
import Numeric (showHex)

-- | Serialize an event to canonical JSON bytes.
serializeEvent :: Event -> ByteString
serializeEvent =
    BSL.toStrict
        . encodingToLazyByteString
        . encodeEvent

-- | Encode an event as an aeson 'Encoding'.
encodeEvent :: Event -> Encoding
encodeEvent = \case
    Inception d -> encodeInception d
    Rotation d -> encodeRotation d
    Interaction d -> encodeInteraction d
    Receipt d -> encodeReceipt d

encodeInception :: InceptionData -> Encoding
encodeInception
    InceptionData
        { version
        , digest
        , prefix
        , sequenceNumber
        , signingThreshold
        , keys
        , nextThreshold
        , nextKeys
        , witnessThreshold
        , witnesses
        , config
        , anchors
        } =
        pairs $
            mconcat
                [ pair "v" (text version)
                , pair "t" (text "icp")
                , pair "d" (text digest)
                , pair "i" (text prefix)
                , pair "s" (text $ toHex sequenceNumber)
                , pair "kt" (text $ toStr signingThreshold)
                , pair "k" (list text keys)
                , pair "nt" (text $ toStr nextThreshold)
                , pair "n" (list text nextKeys)
                , pair "bt" (text $ toStr witnessThreshold)
                , pair "b" (list text witnesses)
                , pair "c" (list text config)
                , pair "a" (list value anchors)
                ]

encodeRotation :: RotationData -> Encoding
encodeRotation
    RotationData
        { version
        , digest
        , prefix
        , sequenceNumber
        , priorDigest
        , signingThreshold
        , keys
        , nextThreshold
        , nextKeys
        , witnessThreshold
        , witnessesRemoved
        , witnessesAdded
        , config
        , anchors
        } =
        pairs $
            mconcat
                [ pair "v" (text version)
                , pair "t" (text "rot")
                , pair "d" (text digest)
                , pair "i" (text prefix)
                , pair "s" (text $ toHex sequenceNumber)
                , pair "p" (text priorDigest)
                , pair "kt" (text $ toStr signingThreshold)
                , pair "k" (list text keys)
                , pair "nt" (text $ toStr nextThreshold)
                , pair "n" (list text nextKeys)
                , pair "bt" (text $ toStr witnessThreshold)
                , pair "ba" (list text witnessesAdded)
                , pair "br" (list text witnessesRemoved)
                , pair "c" (list text config)
                , pair "a" (list value anchors)
                ]

encodeInteraction :: InteractionData -> Encoding
encodeInteraction
    InteractionData
        { version
        , digest
        , prefix
        , sequenceNumber
        , priorDigest
        , anchors
        } =
        pairs $
            mconcat
                [ pair "v" (text version)
                , pair "t" (text "ixn")
                , pair "d" (text digest)
                , pair "i" (text prefix)
                , pair "s" (text $ toHex sequenceNumber)
                , pair "p" (text priorDigest)
                , pair "a" (list value anchors)
                ]

encodeReceipt :: ReceiptData -> Encoding
encodeReceipt ReceiptData{version, digest, prefix, sequenceNumber} =
    pairs $
        mconcat
            [ pair "v" (text version)
            , pair "t" (text "rct")
            , pair "d" (text digest)
            , pair "i" (text prefix)
            , pair "s" (text $ toHex sequenceNumber)
            ]

toHex :: Int -> T.Text
toHex n = T.pack (showHex n "")

toStr :: Int -> T.Text
toStr = T.pack . show
