module Keri.Event
    ( Event (..)
    , EventType (..)
    , InceptionData (..)
    , RotationData (..)
    , InteractionData (..)
    , ReceiptData (..)
    , eventTypeText
    , eventType
    , eventDigest
    , eventPrefix
    , eventSequenceNumber
    ) where

-- \|
-- Module      : Keri.Event
-- Description : KERI event types
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Defines the core event sum type and data records for
-- each KERI event kind: inception, rotation, interaction,
-- and receipt.

import Data.Aeson (Value)
import Data.Text (Text)

-- | KERI event type identifiers.
data EventType
    = Icp
    | Rot
    | Ixn
    | Rct
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Text representation for JSON serialization.
eventTypeText :: EventType -> Text
eventTypeText = \case
    Icp -> "icp"
    Rot -> "rot"
    Ixn -> "ixn"
    Rct -> "rct"

-- | Sum type of all KERI events.
data Event
    = Inception InceptionData
    | Rotation RotationData
    | Interaction InteractionData
    | Receipt ReceiptData
    deriving stock (Show, Eq)

-- | Inception event data (type @icp@).
data InceptionData = InceptionData
    { version :: Text
    , digest :: Text
    , prefix :: Text
    , sequenceNumber :: Int
    , signingThreshold :: Int
    , keys :: [Text]
    , nextThreshold :: Int
    , nextKeys :: [Text]
    , witnessThreshold :: Int
    , witnesses :: [Text]
    , config :: [Text]
    , anchors :: [Value]
    }
    deriving stock (Show, Eq)

-- | Rotation event data (type @rot@).
data RotationData = RotationData
    { version :: Text
    , digest :: Text
    , prefix :: Text
    , sequenceNumber :: Int
    , priorDigest :: Text
    , signingThreshold :: Int
    , keys :: [Text]
    , nextThreshold :: Int
    , nextKeys :: [Text]
    , witnessThreshold :: Int
    , witnessesRemoved :: [Text]
    , witnessesAdded :: [Text]
    , config :: [Text]
    , anchors :: [Value]
    }
    deriving stock (Show, Eq)

-- | Interaction event data (type @ixn@).
data InteractionData = InteractionData
    { version :: Text
    , digest :: Text
    , prefix :: Text
    , sequenceNumber :: Int
    , priorDigest :: Text
    , anchors :: [Value]
    }
    deriving stock (Show, Eq)

-- | Receipt event data (type @rct@).
data ReceiptData = ReceiptData
    { version :: Text
    , digest :: Text
    , prefix :: Text
    , sequenceNumber :: Int
    }
    deriving stock (Show, Eq)

-- | Extract the event type from an event.
eventType :: Event -> EventType
eventType = \case
    Inception{} -> Icp
    Rotation{} -> Rot
    Interaction{} -> Ixn
    Receipt{} -> Rct

-- | Extract the SAID digest from an event.
eventDigest :: Event -> Text
eventDigest = \case
    Inception InceptionData{digest} -> digest
    Rotation RotationData{digest} -> digest
    Interaction InteractionData{digest} -> digest
    Receipt ReceiptData{digest} -> digest

-- | Extract the prefix from an event.
eventPrefix :: Event -> Text
eventPrefix = \case
    Inception InceptionData{prefix} -> prefix
    Rotation RotationData{prefix} -> prefix
    Interaction InteractionData{prefix} -> prefix
    Receipt ReceiptData{prefix} -> prefix

-- | Extract the sequence number from an event.
eventSequenceNumber :: Event -> Int
eventSequenceNumber = \case
    Inception InceptionData{sequenceNumber} ->
        sequenceNumber
    Rotation RotationData{sequenceNumber} ->
        sequenceNumber
    Interaction InteractionData{sequenceNumber} ->
        sequenceNumber
    Receipt ReceiptData{sequenceNumber} ->
        sequenceNumber
