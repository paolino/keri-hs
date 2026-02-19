module Keri.Kel
    ( Kel (..)
    , SignedEvent (..)
    , emptyKel
    ) where

{- |
Module      : Keri.Kel
Description : Key Event Log types
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

A Key Event Log (KEL) is an ordered sequence of signed
events forming the verifiable history of an identifier.
-}

import Data.Text (Text)
import Keri.Event (Event)

-- | A signed event: an event with indexed signatures.
data SignedEvent = SignedEvent
    { event :: Event
    , signatures :: [(Int, Text)]
    }
    deriving stock (Show, Eq)

{- | A Key Event Log: ordered list of signed events.
The first event must be an inception.
-}
newtype Kel = Kel {unKel :: [SignedEvent]}
    deriving stock (Show, Eq)

-- | An empty KEL with no events.
emptyKel :: Kel
emptyKel = Kel []
