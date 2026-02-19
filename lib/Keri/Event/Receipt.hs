module Keri.Event.Receipt
    ( ReceiptConfig (..)
    , mkReceipt
    ) where

{- |
Module      : Keri.Event.Receipt
Description : Receipt event construction
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Constructs receipt events for direct-mode
acknowledgement of key events.
-}

import Data.ByteString qualified as BS
import Data.Text (Text)
import Keri.Crypto.Digest
    ( computeSaid
    , saidPlaceholder
    )
import Keri.Event
    ( Event (..)
    , ReceiptData (..)
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Event.Version
    ( mkVersion
    , versionPlaceholder
    )

-- | Configuration for creating a receipt event.
data ReceiptConfig = ReceiptConfig
    { prefix :: Text
    , sequenceNumber :: Int
    }
    deriving stock (Show, Eq)

{- | Create a receipt event. Computes the version
string size and SAID automatically.
-}
mkReceipt :: ReceiptConfig -> Event
mkReceipt cfg = Receipt finalData
  where
    placeholder =
        ReceiptData
            { version = versionPlaceholder
            , digest = saidPlaceholder
            , prefix =
                prefix (cfg :: ReceiptConfig)
            , sequenceNumber =
                sequenceNumber
                    (cfg :: ReceiptConfig)
            }
    size0 =
        BS.length $
            serializeEvent (Receipt placeholder)
    realVersion = mkVersion size0
    withVersion =
        placeholder{version = realVersion}
    saidBytes =
        serializeEvent (Receipt withVersion)
    said = computeSaid saidBytes
    finalData = withVersion{digest = said}
