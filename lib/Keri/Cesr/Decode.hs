module Keri.Cesr.Decode
    ( decode
    ) where

{- |
Module      : Keri.Cesr.Decode
Description : Base64url text to CESR primitive
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Decodes a CESR-encoded text string back into a
'Primitive'. Identifies the code from the prefix,
reconstructs the zero-pad, Base64url-decodes, and
strips the padding to recover raw bytes.
-}

import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Keri.Cesr.DerivationCode
    ( codeLength
    , identifyCode
    , totalLength
    )
import Keri.Cesr.Primitive (Primitive (..))

-- | Decode a CESR-encoded text to a 'Primitive'.
decode :: Text -> Either String Primitive
decode txt = do
    c <- identifyCode txt
    let padLen = codeLength c
        expected = totalLength c
    if T.length txt < expected
        then
            Left $
                "Too short: expected "
                    <> show expected
                    <> " chars, got "
                    <> show (T.length txt)
        else do
            let body = T.take expected txt
                zeroPad = T.replicate padLen "A"
                b64Text = zeroPad <> T.drop padLen body
            rawWithPad <-
                B64.decode (TE.encodeUtf8 b64Text)
            pure
                Primitive
                    { code = c
                    , raw = BS.drop padLen rawWithPad
                    }
