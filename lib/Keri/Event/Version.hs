module Keri.Event.Version
    ( mkVersion
    , versionPlaceholder
    , parseVersionSize
    , versionPrefix
    ) where

{- |
Module      : Keri.Event.Version
Description : KERI version string handling
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

The KERI version string format is:

@KERI10JSON{size:06x}_@

where @size@ is the total byte count of the serialized
JSON message, encoded as 6 hex digits.
-}

import Data.Text (Text)
import Data.Text qualified as T
import Numeric (readHex, showHex)

-- | Fixed version string prefix.
versionPrefix :: Text
versionPrefix = "KERI10JSON"

-- | Build a version string with the given byte size.
mkVersion :: Int -> Text
mkVersion size =
    versionPrefix <> padHex 6 size <> "_"

{- | Placeholder version string with zero size,
used during SAID computation.
-}
versionPlaceholder :: Text
versionPlaceholder = mkVersion 0

{- | Parse the byte size from a version string.
Returns 'Nothing' if the format is invalid.
-}
parseVersionSize :: Text -> Maybe Int
parseVersionSize v = do
    rest <- T.stripPrefix versionPrefix v
    hexPart <- T.stripSuffix "_" rest
    case readHex (T.unpack hexPart) of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Pad an integer as hex to the given width.
padHex :: Int -> Int -> Text
padHex width n =
    let hex = T.pack (showHex n "")
        padding =
            T.replicate (width - T.length hex) "0"
    in padding <> hex
