module Keri.Cesr.Encode
    ( encode
    ) where

{- |
Module      : Keri.Cesr.Encode
Description : CESR primitive to Base64url text
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Encodes a 'Primitive' into its CESR text representation.
The algorithm prepends zero-pad bytes (count = code length),
Base64url-encodes, then replaces the leading characters
with the code text.
-}

import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Keri.Cesr.DerivationCode (codeLength, codeText)
import Keri.Cesr.Primitive (Primitive (..))

-- | Encode a 'Primitive' to CESR Base64url text.
encode :: Primitive -> Text
encode Primitive{code, raw} =
    let padLen = codeLength code
        padded = BS.replicate padLen 0 <> raw
        b64 = TE.decodeLatin1 (B64.encode padded)
    in codeText code <> T.drop padLen b64
