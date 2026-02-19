module Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    ) where

-- \|
-- Module      : Keri.Event.Inception
-- Description : Inception event construction
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Constructs inception events with computed SAID
-- and version string. The prefix of an inception event
-- equals its SAID (self-addressing identifier).

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Keri.Crypto.Digest
    ( computeSaid
    , saidPlaceholder
    )
import Keri.Event
    ( Event (..)
    , InceptionData (..)
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Event.Version
    ( mkVersion
    , versionPlaceholder
    )

-- | Configuration for creating an inception event.
data InceptionConfig = InceptionConfig
    { icKeys :: [Text]
    , icSigningThreshold :: Int
    , icNextKeys :: [Text]
    , icNextThreshold :: Int
    , icConfig :: [Text]
    , icAnchors :: [Value]
    }
    deriving stock (Show, Eq)

{- | Create an inception event. Computes the version
string size and SAID automatically. The prefix @i@ is
set equal to the SAID @d@ (self-addressing).
-}
mkInception :: InceptionConfig -> Event
mkInception InceptionConfig{..} =
    Inception finalData
  where
    placeholder =
        InceptionData
            { version = versionPlaceholder
            , digest = saidPlaceholder
            , prefix = saidPlaceholder
            , sequenceNumber = 0
            , signingThreshold = icSigningThreshold
            , keys = icKeys
            , nextThreshold = icNextThreshold
            , nextKeys = icNextKeys
            , witnessThreshold = 0
            , witnesses = []
            , config = icConfig
            , anchors = icAnchors
            }
    size0 =
        BS.length $
            serializeEvent (Inception placeholder)
    realVersion = mkVersion size0
    withVersion =
        placeholder{version = realVersion}
    saidBytes =
        serializeEvent (Inception withVersion)
    said = computeSaid saidBytes
    finalData =
        withVersion
            { digest = said
            , prefix = said
            }
