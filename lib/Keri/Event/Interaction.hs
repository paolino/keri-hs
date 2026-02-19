module Keri.Event.Interaction
    ( InteractionConfig (..)
    , mkInteraction
    ) where

{- |
Module      : Keri.Event.Interaction
Description : Interaction event construction
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Constructs interaction events with computed SAID and
version string. Interactions anchor data without
changing keys.
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
    , InteractionData (..)
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Event.Version
    ( mkVersion
    , versionPlaceholder
    )

-- | Configuration for creating an interaction event.
data InteractionConfig = InteractionConfig
    { prefix :: Text
    , sequenceNumber :: Int
    , priorDigest :: Text
    , anchors :: [Value]
    }
    deriving stock (Show, Eq)

{- | Create an interaction event. Computes the version
string size and SAID automatically.
-}
mkInteraction :: InteractionConfig -> Event
mkInteraction cfg = Interaction finalData
  where
    placeholder =
        InteractionData
            { version = versionPlaceholder
            , digest = saidPlaceholder
            , prefix =
                prefix
                    (cfg :: InteractionConfig)
            , sequenceNumber =
                sequenceNumber
                    (cfg :: InteractionConfig)
            , priorDigest =
                priorDigest
                    (cfg :: InteractionConfig)
            , anchors =
                anchors
                    (cfg :: InteractionConfig)
            }
    size0 =
        BS.length $
            serializeEvent
                (Interaction placeholder)
    realVersion = mkVersion size0
    withVersion =
        placeholder{version = realVersion}
    saidBytes =
        serializeEvent (Interaction withVersion)
    said = computeSaid saidBytes
    finalData = withVersion{digest = said}
