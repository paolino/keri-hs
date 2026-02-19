module Keri.Kel.Replay
    ( replay
    ) where

{- |
Module      : Keri.Kel.Replay
Description : Replay a KEL to derive key state
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Replays all events in a KEL to compute the current
key state. Does not re-verify signatures (that is
done during append).
-}

import Keri.Event (Event (..), InceptionData)
import Keri.Kel (Kel (..), SignedEvent (..))
import Keri.KeyState (KeyState, applyEvent, initialState)

{- | Replay a KEL from the beginning to derive the
current 'KeyState'. Returns an error if the KEL is
empty or contains invalid event sequences.
-}
replay :: Kel -> Either String KeyState
replay (Kel []) = Left "Empty KEL"
replay (Kel (first' : rest)) =
    case event first' of
        Inception d ->
            foldEvents (initialState d) rest
        _ -> Left "KEL must start with inception"

foldEvents
    :: KeyState
    -> [SignedEvent]
    -> Either String KeyState
foldEvents ks [] = Right ks
foldEvents ks (se : rest) = do
    ks' <- applyEvent ks (event se)
    foldEvents ks' rest
