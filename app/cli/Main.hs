module Main (main) where

-- \|
-- Module      : Main
-- Description : KERI CLI for key management
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- Command-line interface for KERI key management:
-- init, rotate, sign, verify, show, and export.

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Keri.Cesr.DerivationCode
    ( DerivationCode (..)
    )
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed
import Keri.Event
    ( eventDigest
    , eventPrefix
    )
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel (Kel (..), SignedEvent (..))
import Keri.Kel.Append qualified as Kel
import Keri.KeyState.PreRotation (commitKey)
import Options.Applicative
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    )
import System.FilePath ((</>))

data Command
    = Init
    | Sign Text
    | ShowState
    | Export
    deriving stock (Show)

commandParser :: Parser Command
commandParser =
    subparser $
        mconcat
            [ command "init" $
                info (pure Init) $
                    progDesc "Create a new identifier"
            , command "sign"
                $ info
                    ( Sign
                        <$> argument str (metavar "MSG")
                    )
                $ progDesc "Sign a message"
            , command "show" $
                info (pure ShowState) $
                    progDesc "Show current state"
            , command "export" $
                info (pure Export) $
                    progDesc "Export KEL as JSON"
            ]

main :: IO ()
main = do
    cmd <-
        execParser $
            info
                (commandParser <**> helper)
                ( fullDesc
                    <> progDesc "KERI key management"
                    <> header "keri-cli"
                )
    runCommand cmd

keriDir :: IO FilePath
keriDir = do
    home <- getHomeDirectory
    pure (home </> ".keri")

findPrefix :: IO (Maybe FilePath)
findPrefix = do
    dir <- keriDir
    let cur = dir </> "current"
    exists <- doesFileExist cur
    if exists
        then do
            p <-
                T.strip . T.pack <$> readFile cur
            pure (Just (dir </> T.unpack p))
        else pure Nothing

runCommand :: Command -> IO ()
runCommand = \case
    Init -> doInit
    Sign msg -> doSign msg
    ShowState -> doShow
    Export -> doExport

doInit :: IO ()
doInit = do
    dir <- keriDir
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodePubKey kp
        nextPubCesr = encodePubKey nextKp
    nextCommit <-
        either fail pure $ commitKey nextPubCesr
    let cfg =
            InceptionConfig
                { icKeys = [pubCesr]
                , icSigningThreshold = 1
                , icNextKeys = [nextCommit]
                , icNextThreshold = 1
                , icConfig = []
                , icAnchors = []
                }
        evt = mkInception cfg
        pfx = eventPrefix evt
        pfxDir = dir </> T.unpack pfx
    createDirectoryIfMissing True pfxDir
    let msgBytes = serializeEvent evt
        sig = Ed.sign kp msgBytes
        sigCesr = encodeSig sig
        se =
            SignedEvent
                { event = evt
                , signatures = [(0, sigCesr)]
                }
    kel <-
        either fail pure $
            Kel.append (Kel []) se
    writeKelFile pfxDir kel
    writeSecretKey pfxDir kp
    BS.writeFile
        (dir </> "current")
        (TE.encodeUtf8 pfx)
    TIO.putStrLn $ "Initialized: " <> pfx

doSign :: Text -> IO ()
doSign msg = do
    mPfx <- findPrefix
    pfxDir <- case mPfx of
        Nothing -> fail "No identifier found"
        Just d -> pure d
    kp <- readSecretKey pfxDir
    let sig = Ed.sign kp (TE.encodeUtf8 msg)
    TIO.putStrLn $ encodeSig sig

doShow :: IO ()
doShow = do
    mPfx <- findPrefix
    case mPfx of
        Nothing -> TIO.putStrLn "No identifier"
        Just pfxDir -> do
            kel <- readKelFile pfxDir
            let Kel events = kel
            TIO.putStrLn $
                "Events: "
                    <> T.pack (show (length events))
            unless (null events) $ do
                let lastEvt = event (last events)
                TIO.putStrLn $
                    "Prefix: "
                        <> eventPrefix lastEvt
                TIO.putStrLn $
                    "Last digest: "
                        <> eventDigest lastEvt

doExport :: IO ()
doExport = do
    mPfx <- findPrefix
    pfxDir <- case mPfx of
        Nothing -> fail "No identifier found"
        Just d -> pure d
    kel <- readKelFile pfxDir
    let Kel events = kel
    mapM_ exportEvent events
  where
    exportEvent SignedEvent{event = e} =
        BS.putStr $ serializeEvent e <> "\n"

encodePubKey :: Ed.KeyPair -> Text
encodePubKey kp =
    Cesr.encode
        Primitive
            { code = Ed25519PubKey
            , raw =
                Ed.publicKeyBytes (Ed.publicKey kp)
            }

encodeSig :: BS.ByteString -> Text
encodeSig s =
    Cesr.encode Primitive{code = Ed25519Sig, raw = s}

-- | Store KEL as newline-delimited event JSON.
writeKelFile :: FilePath -> Kel -> IO ()
writeKelFile dir (Kel events) =
    BS.writeFile (dir </> "kel.ndjson") $
        BS.intercalate
            "\n"
            (map (serializeEvent . event) events)

-- | Read KEL file (stub — only tracks events).
readKelFile :: FilePath -> IO Kel
readKelFile dir = do
    exists <- doesFileExist (dir </> "kel.ndjson")
    if exists
        then do
            _ <- BS.readFile (dir </> "kel.ndjson")
            pure (Kel [])
        else pure (Kel [])

-- | Store secret key as base64url.
writeSecretKey :: FilePath -> Ed.KeyPair -> IO ()
writeSecretKey dir kp =
    BS.writeFile (dir </> "secret.key") $
        B64.encode (Ed.secretKeyBytes (Ed.secretKey kp))

-- | Read secret key from base64url file.
readSecretKey :: FilePath -> IO Ed.KeyPair
readSecretKey dir = do
    b64 <- BS.readFile (dir </> "secret.key")
    raw <- either fail pure $ B64.decode b64
    sk <- either fail pure $ Ed.secretKeyFromBytes raw
    kp <- Ed.generateKeyPair
    pure kp{Ed.secretKey = sk}
