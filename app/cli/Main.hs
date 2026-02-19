module Main (main) where

{- |
Module      : Main
Description : KERI CLI for key management
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Command-line interface for KERI key management:
init, rotate, sign, verify, show, and export.
-}

import Control.Monad (unless, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
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
    ( Event (..)
    , InceptionData (..)
    , eventDigest
    , eventPrefix
    )
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Rotation
    ( RotationConfig (..)
    , mkRotation
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
    , listDirectory
    )
import System.FilePath ((</>))

data Command
    = Init
    | Rotate
    | Sign Text
    | Verify Text Text
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
            , command "rotate" $
                info (pure Rotate) $
                    progDesc "Rotate signing keys"
            , command "sign" $
                info (Sign <$> argument str (metavar "MSG")) $
                    progDesc "Sign a message"
            , command "verify" $
                info
                    ( Verify
                        <$> argument str (metavar "MSG")
                        <*> argument str (metavar "SIG")
                    )
                    $ progDesc "Verify a signature"
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

-- | Resolve the storage directory for a prefix.
keriDir :: IO FilePath
keriDir = do
    home <- getHomeDirectory
    pure (home </> ".keri")

-- | Find the first (and only) prefix directory.
findPrefix :: IO (Maybe FilePath)
findPrefix = do
    dir <- keriDir
    exists <- doesFileExist (dir </> "current")
    if exists
        then do
            p <- readFile (dir </> "current")
            pure (Just (dir </> T.unpack (T.strip (T.pack p))))
        else do
            e <-
                doesFileExist
                    (dir </> "placeholder")
            if e
                then pure Nothing
                else do
                    entries <-
                        listDirectory dir
                    case entries of
                        [d'] -> pure (Just (dir </> d'))
                        _ -> pure Nothing

runCommand :: Command -> IO ()
runCommand = \case
    Init -> doInit
    Rotate -> doRotate
    Sign msg -> doSign msg
    Verify msg sig -> doVerify msg sig
    ShowState -> doShow
    Export -> doExport

doInit :: IO ()
doInit = do
    dir <- keriDir
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodePubKey kp
        nextPubCesr = encodePubKey nextKp
    nextCommit <- either fail pure $ commitKey nextPubCesr
    let cfg =
            InceptionConfig
                { keys = [pubCesr]
                , signingThreshold = 1
                , nextKeys = [nextCommit]
                , nextThreshold = 1
                , config = []
                , anchors = []
                }
        evt = mkInception cfg
        pfx = eventPrefix evt
        pfxDir = dir </> T.unpack pfx
    createDirectoryIfMissing True pfxDir
    let msgBytes = serializeEvent (unwrapEvent evt)
        sig = Ed.sign kp msgBytes
        sigCesr = encodeSig sig
        se =
            SignedEvent
                { event = evt
                , signatures = [(0, sigCesr)]
                }
    writeKel pfxDir (Kel [se])
    writeKeys pfxDir [kp]
    writeNextKeys pfxDir [nextKp]
    BS.writeFile
        (dir </> "current")
        (TE.encodeUtf8 pfx)
    TIO.putStrLn $ "Initialized: " <> pfx

doRotate :: IO ()
doRotate = do
    dir <- keriDir
    mPfx <- findPrefix
    pfxDir <- case mPfx of
        Nothing -> fail "No identifier found"
        Just d -> pure d
    kel <- readKel pfxDir
    currentKeys <- readKeys pfxDir
    nextKeys' <- readNextKeys pfxDir
    newNextKp <- Ed.generateKeyPair
    let newPubCesr = encodePubKey (head nextKeys')
        newNextPubCesr = encodePubKey newNextKp
    newNextCommit <-
        either fail pure $ commitKey newNextPubCesr
    let Kel events = kel
        lastEvt = event (last events)
        pfx = eventPrefix lastEvt
        sn = length events
        prior = eventDigest lastEvt
        cfg =
            RotationConfig
                { prefix = pfx
                , sequenceNumber = sn
                , priorDigest = prior
                , keys = [newPubCesr]
                , signingThreshold = 1
                , nextKeys = [newNextCommit]
                , nextThreshold = 1
                , config = []
                , anchors = []
                }
        evt = mkRotation cfg
        msgBytes = serializeEvent (unwrapEvent evt)
        kp = head currentKeys
        sig = Ed.sign kp msgBytes
        sigCesr = encodeSig sig
        se =
            SignedEvent
                { event = evt
                , signatures = [(0, sigCesr)]
                }
    newKel <- either fail pure $ Kel.append kel se
    writeKel pfxDir newKel
    writeKeys pfxDir nextKeys'
    writeNextKeys pfxDir [newNextKp]
    TIO.putStrLn $ "Rotated: sequence " <> T.pack (show sn)

doSign :: Text -> IO ()
doSign msg = do
    mPfx <- findPrefix
    pfxDir <- case mPfx of
        Nothing -> fail "No identifier found"
        Just d -> pure d
    currentKeys <- readKeys pfxDir
    let kp = head currentKeys
        sig = Ed.sign kp (TE.encodeUtf8 msg)
    TIO.putStrLn $ encodeSig sig

doVerify :: Text -> Text -> IO ()
doVerify msg sig = do
    mPfx <- findPrefix
    pfxDir <- case mPfx of
        Nothing -> fail "No identifier found"
        Just d -> pure d
    currentKeys <- readKeys pfxDir
    let kp = head currentKeys
        msgBytes = TE.encodeUtf8 msg
        sigBytes = TE.encodeUtf8 sig
        valid =
            Ed.verify
                (Ed.publicKey kp)
                msgBytes
                (BS.take 64 sigBytes)
    TIO.putStrLn $
        if valid then "Valid" else "Invalid"

doShow :: IO ()
doShow = do
    mPfx <- findPrefix
    case mPfx of
        Nothing -> TIO.putStrLn "No identifier"
        Just pfxDir -> do
            kel <- readKel pfxDir
            let Kel events = kel
            TIO.putStrLn $
                "Events: "
                    <> T.pack (show (length events))
            when (not (null events)) $ do
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
    kel <- readKel pfxDir
    let Kel events = kel
    mapM_ exportEvent events
  where
    exportEvent SignedEvent{event} =
        BS.putStr $ serializeEvent (unwrapEvent event) <> "\n"

unwrapEvent :: Event -> Event
unwrapEvent = id

encodePubKey :: Ed.KeyPair -> Text
encodePubKey kp =
    Cesr.encode
        Primitive
            { code = Ed25519PubKey
            , raw = Ed.publicKeyBytes (Ed.publicKey kp)
            }

encodeSig :: BS.ByteString -> Text
encodeSig sig =
    Cesr.encode
        Primitive
            { code = Ed25519Sig
            , raw = sig
            }

-- Simplified JSON storage using event serialization

writeKel :: FilePath -> Kel -> IO ()
writeKel dir (Kel events) = do
    let encoded = map encodeSignedEvent events
        json = Aeson.encode encoded
    BSL.writeFile (dir </> "kel.json") json

readKel :: FilePath -> IO Kel
readKel dir = do
    exists <- doesFileExist (dir </> "kel.json")
    unless exists $ fail "No KEL found"
    bs <- BSL.readFile (dir </> "kel.json")
    case Aeson.decode bs of
        Nothing -> fail "Invalid KEL JSON"
        Just vals -> pure (Kel (map decodeSignedEvent vals))

encodeSignedEvent :: SignedEvent -> Aeson.Value
encodeSignedEvent SignedEvent{event, signatures} =
    let eventBytes = serializeEvent event
    in Aeson.object
        [ "event"
            Aeson..= TE.decodeUtf8 eventBytes
        , "signatures"
            Aeson..= map
                ( \(i, s) ->
                    Aeson.object
                        [ "index" Aeson..= i
                        , "signature" Aeson..= s
                        ]
                )
                signatures
        ]

decodeSignedEvent :: Aeson.Value -> SignedEvent
decodeSignedEvent _ =
    error "KEL deserialization not yet implemented"

writeKeys :: FilePath -> [Ed.KeyPair] -> IO ()
writeKeys dir kps = do
    let encoded =
            map
                ( \kp ->
                    TE.decodeUtf8
                        (Ed.secretKeyBytes (Ed.secretKey kp))
                )
                kps
    BSL.writeFile (dir </> "keys.json") $
        Aeson.encode encoded

readKeys :: FilePath -> IO [Ed.KeyPair]
readKeys dir = do
    exists <- doesFileExist (dir </> "keys.json")
    unless exists $ fail "No keys found"
    bs <- BSL.readFile (dir </> "keys.json")
    case Aeson.decode bs of
        Nothing -> fail "Invalid keys JSON"
        Just (texts :: [BS.ByteString]) ->
            mapM keyPairFromSecret texts

writeNextKeys :: FilePath -> [Ed.KeyPair] -> IO ()
writeNextKeys dir kps = do
    let encoded =
            map
                ( \kp ->
                    TE.decodeUtf8
                        (Ed.secretKeyBytes (Ed.secretKey kp))
                )
                kps
    BSL.writeFile (dir </> "next-keys.json") $
        Aeson.encode encoded

readNextKeys :: FilePath -> IO [Ed.KeyPair]
readNextKeys dir = do
    exists <-
        doesFileExist (dir </> "next-keys.json")
    unless exists $ fail "No next keys found"
    bs <- BSL.readFile (dir </> "next-keys.json")
    case Aeson.decode bs of
        Nothing -> fail "Invalid next-keys JSON"
        Just (texts :: [BS.ByteString]) ->
            mapM keyPairFromSecret texts

keyPairFromSecret
    :: BS.ByteString -> IO Ed.KeyPair
keyPairFromSecret bs = do
    sk <- either fail pure $ Ed.secretKeyFromBytes bs
    pure
        Ed.KeyPair
            { Ed.secretKey = sk
            , Ed.publicKey =
                error "reconstruct from sk"
            }
