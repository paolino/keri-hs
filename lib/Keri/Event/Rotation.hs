module Keri.Event.Rotation
    ( RotationConfig (..)
    , mkRotation
    ) where

{- |
Module      : Keri.Event.Rotation
Description : Rotation event construction
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Constructs rotation events with computed SAID and
version string. Rotation changes the current signing
keys and establishes new pre-rotation commitments.
-}

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Keri.Crypto.Digest
    ( computeSaid
    , saidPlaceholder
    )
import Keri.Event
    ( Event (..)
    , RotationData (..)
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Event.Version
    ( mkVersion
    , versionPlaceholder
    )

-- | Configuration for creating a rotation event.
data RotationConfig = RotationConfig
    { prefix :: Text
    , sequenceNumber :: Int
    , priorDigest :: Text
    , keys :: [Text]
    , signingThreshold :: Int
    , nextKeys :: [Text]
    , nextThreshold :: Int
    , config :: [Text]
    , anchors :: [Value]
    }
    deriving stock (Show, Eq)

{- | Create a rotation event. Computes the version
string size and SAID automatically.
-}
mkRotation :: RotationConfig -> Event
mkRotation cfg = Rotation finalData
  where
    placeholder =
        RotationData
            { version = versionPlaceholder
            , digest = saidPlaceholder
            , prefix =
                prefix (cfg :: RotationConfig)
            , sequenceNumber =
                sequenceNumber
                    (cfg :: RotationConfig)
            , priorDigest =
                priorDigest
                    (cfg :: RotationConfig)
            , signingThreshold =
                signingThreshold
                    (cfg :: RotationConfig)
            , keys =
                keys (cfg :: RotationConfig)
            , nextThreshold =
                nextThreshold
                    (cfg :: RotationConfig)
            , nextKeys =
                nextKeys
                    (cfg :: RotationConfig)
            , witnessThreshold = 0
            , witnessesRemoved = []
            , witnessesAdded = []
            , config =
                config (cfg :: RotationConfig)
            , anchors =
                anchors (cfg :: RotationConfig)
            }
    size0 =
        BS.length $
            serializeEvent (Rotation placeholder)
    realVersion = mkVersion size0
    withVersion =
        placeholder{version = realVersion}
    saidBytes =
        serializeEvent (Rotation withVersion)
    said = computeSaid saidBytes
    finalData = withVersion{digest = said}
