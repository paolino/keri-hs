module Keri.Kel.Append
    ( append
    ) where

{- |
Module      : Keri.Kel.Append
Description : Append events to a KEL
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Appends a signed event to a KEL after verifying
signatures and chain integrity.
-}

import Keri.Event
    ( Event (..)
    , eventSequenceNumber
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel (Kel (..), SignedEvent (..))
import Keri.Kel.Replay (replay)
import Keri.KeyState (applyEvent, initialState)
import Keri.KeyState qualified as KS
import Keri.KeyState.Verify (verifySignatures)

{- | Append a signed event to the KEL. Verifies:

1. Signatures meet the current threshold
2. Event is consistent with current key state
-}
append
    :: Kel -> SignedEvent -> Either String Kel
append kel@(Kel events) se@SignedEvent{..} =
    case events of
        [] -> appendInception se
        _ -> appendToExisting kel se

appendInception
    :: SignedEvent -> Either String Kel
appendInception se@SignedEvent{event, signatures} =
    case event of
        Inception d -> do
            let msgBytes = serializeEvent event
                ks = initialState d
            verified <-
                verifySignatures
                    (KS.stateKeys ks)
                    (KS.stateSigningThreshold ks)
                    msgBytes
                    signatures
            if verified
                then Right (Kel [se])
                else Left "Inception signatures invalid"
        _ ->
            Left "First event must be inception"

appendToExisting
    :: Kel -> SignedEvent -> Either String Kel
appendToExisting (Kel events) se@SignedEvent{..} = do
    ks <- replay (Kel events)
    let msgBytes = serializeEvent event
    verified <-
        verifySignatures
            (KS.stateKeys ks)
            (KS.stateSigningThreshold ks)
            msgBytes
            signatures
    if not verified
        then Left "Signatures invalid"
        else do
            let expectedSn =
                    KS.stateSequenceNumber ks + 1
                actualSn = eventSequenceNumber event
            if actualSn /= expectedSn
                then
                    Left $
                        "Expected sequence "
                            <> show expectedSn
                            <> ", got "
                            <> show actualSn
                else do
                    _ <- applyEvent ks event
                    Right (Kel (events ++ [se]))
