module Keri.Event.Rotation
    ( RotationConfig (..)
    , mkRotation
    ) where

-- \|
-- Module      : Keri.Event.Rotation
-- Description : Rotation event construction
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Constructs rotation events with computed SAID and
-- version string. Rotation changes the current signing
-- keys and establishes new pre-rotation commitments.

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
    { rcPrefix :: Text
    , rcSequenceNumber :: Int
    , rcPriorDigest :: Text
    , rcKeys :: [Text]
    , rcSigningThreshold :: Int
    , rcNextKeys :: [Text]
    , rcNextThreshold :: Int
    , rcConfig :: [Text]
    , rcAnchors :: [Value]
    }
    deriving stock (Show, Eq)

{- | Create a rotation event. Computes the version
string size and SAID automatically.
-}
mkRotation :: RotationConfig -> Event
mkRotation RotationConfig{..} = Rotation finalData
  where
    placeholder =
        RotationData
            { version = versionPlaceholder
            , digest = saidPlaceholder
            , prefix = rcPrefix
            , sequenceNumber = rcSequenceNumber
            , priorDigest = rcPriorDigest
            , signingThreshold = rcSigningThreshold
            , keys = rcKeys
            , nextThreshold = rcNextThreshold
            , nextKeys = rcNextKeys
            , witnessThreshold = 0
            , witnessesRemoved = []
            , witnessesAdded = []
            , config = rcConfig
            , anchors = rcAnchors
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
