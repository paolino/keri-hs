module Keri.Crypto.SAID
    ( verifySaid
    , replaceDigest
    ) where

-- \|
-- Module      : Keri.Crypto.SAID
-- Description : SAID (Self-Addressing Identifier) verification
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Verifies that an event's claimed digest matches the
-- recomputed SAID. For inception events, both the digest
-- and prefix must be replaced with the placeholder before
-- hashing, because 'mkInception' sets @prefix = placeholder@
-- during SAID computation.

import Keri.Crypto.Digest (computeSaid, saidPlaceholder)
import Keri.Event
    ( Event (..)
    , InceptionData (..)
    , InteractionData (..)
    , ReceiptData (..)
    , RotationData (..)
    , eventDigest
    )
import Keri.Event.Serialize (serializeEvent)

{- | Replace the digest field with the SAID placeholder.
For inception events, both @d@ and @i@ are replaced
(because the prefix is derived from the SAID).
-}
replaceDigest :: Event -> Event
replaceDigest = \case
    Inception InceptionData{..} ->
        Inception
            InceptionData
                { digest = saidPlaceholder
                , prefix = saidPlaceholder
                , ..
                }
    Rotation RotationData{..} ->
        Rotation
            RotationData
                { digest = saidPlaceholder
                , ..
                }
    Interaction InteractionData{..} ->
        Interaction
            InteractionData
                { digest = saidPlaceholder
                , ..
                }
    Receipt ReceiptData{..} ->
        Receipt
            ReceiptData
                { digest = saidPlaceholder
                , ..
                }

{- | Verify that the event's claimed SAID matches the
recomputed one: replace digest with placeholder, serialize,
hash, and compare against the original digest field.
-}
verifySaid :: Event -> Bool
verifySaid evt =
    computeSaid (serializeEvent (replaceDigest evt))
        == eventDigest evt
