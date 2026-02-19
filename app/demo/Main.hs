module Main (main) where

-- \|
-- Module      : Main
-- Description : Two-party direct-mode KERI demo
-- Copyright   : (c) 2026 Cardano Foundation
-- License     : Apache-2.0
--
-- TCP-based demo of direct-mode KERI: two parties
-- exchange KELs and return signed receipts.
-- Uses length-prefixed JSON framing over TCP.

import Control.Concurrent.Async (concurrently_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Keri.Cesr.DerivationCode
    ( DerivationCode (..)
    )
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed
import Keri.Event (eventPrefix)
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel (Kel (..), SignedEvent (..))
import Keri.Kel.Append qualified as Kel
import Keri.KeyState.PreRotation (commitKey)
import Network.Simple.TCP
    ( HostPreference (..)
    , recv
    , send
    )
import Network.Simple.TCP qualified as TCP
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["serve", port] -> doServe port
        ["connect", host, port] ->
            doConnect host port
        _ -> do
            putStrLn "Usage:"
            putStrLn "  keri-demo serve PORT"
            putStrLn
                "  keri-demo connect HOST PORT"

doServe :: String -> IO ()
doServe port = do
    TIO.putStrLn $
        "Listening on port " <> T.pack port
    (kp, kel) <- createIdentity
    TIO.putStrLn $
        "Server prefix: "
            <> getPrefix kel
    TCP.serve (Host "0.0.0.0") port $
        \(sock, addr) -> do
            TIO.putStrLn $
                "Connection from "
                    <> T.pack (show addr)
            concurrently_
                (sendKel sock kp kel)
                (receiveKel sock)

doConnect :: String -> String -> IO ()
doConnect host port = do
    (kp, kel) <- createIdentity
    TIO.putStrLn $
        "Client prefix: "
            <> getPrefix kel
    TCP.connect host port $ \(sock, _) -> do
        TIO.putStrLn "Connected"
        concurrently_
            (sendKel sock kp kel)
            (receiveKel sock)

createIdentity :: IO (Ed.KeyPair, Kel)
createIdentity = do
    kp <- Ed.generateKeyPair
    nextKp <- Ed.generateKeyPair
    let pubCesr = encodePubKey kp
        nextPubCesr = encodePubKey nextKp
    nextCommit <-
        either fail pure $
            commitKey nextPubCesr
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
        msgBytes =
            serializeEvent evt
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
    pure (kp, kel)

sendKel
    :: TCP.Socket
    -> Ed.KeyPair
    -> Kel
    -> IO ()
sendKel sock _kp (Kel events) = do
    let payload =
            BS.intercalate
                "\n"
                ( map
                    (serializeEvent . event)
                    events
                )
    sendFrame sock payload
    TIO.putStrLn "Sent KEL"

receiveKel :: TCP.Socket -> IO ()
receiveKel sock = do
    mFrame <- recvFrame sock
    case mFrame of
        Nothing ->
            TIO.putStrLn "Connection closed"
        Just frame -> do
            TIO.putStrLn $
                "Received KEL ("
                    <> T.pack (show (BS.length frame))
                    <> " bytes)"
            TIO.putStrLn $
                TE.decodeUtf8 frame

sendFrame :: TCP.Socket -> ByteString -> IO ()
sendFrame sock payload = do
    let len = BS.length payload
        header =
            BS8.pack (show len)
                <> "\n"
    send sock (header <> payload)

recvFrame :: TCP.Socket -> IO (Maybe ByteString)
recvFrame sock = do
    mHeader <- recvLine sock
    case mHeader of
        Nothing -> pure Nothing
        Just header ->
            case reads (BS8.unpack header) of
                [(len, "")] -> do
                    payload <- recvExact sock len
                    pure (Just payload)
                _ -> pure Nothing

recvLine :: TCP.Socket -> IO (Maybe ByteString)
recvLine sock = go BS.empty
  where
    go acc = do
        mByte <- recv sock 1
        case mByte of
            Nothing -> pure Nothing
            Just b
                | b == "\n" -> pure (Just acc)
                | otherwise -> go (acc <> b)

recvExact
    :: TCP.Socket
    -> Int
    -> IO ByteString
recvExact sock = go BS.empty
  where
    go acc 0 = pure acc
    go acc remaining = do
        mChunk <- recv sock remaining
        case mChunk of
            Nothing -> pure acc
            Just chunk ->
                go
                    (acc <> chunk)
                    (remaining - BS.length chunk)

getPrefix :: Kel -> T.Text
getPrefix (Kel []) = "<empty>"
getPrefix (Kel (se : _)) =
    eventPrefix (event se)

encodePubKey :: Ed.KeyPair -> T.Text
encodePubKey kp =
    Cesr.encode
        Primitive
            { code = Ed25519PubKey
            , raw =
                Ed.publicKeyBytes
                    (Ed.publicKey kp)
            }

encodeSig :: ByteString -> T.Text
encodeSig sig =
    Cesr.encode
        Primitive
            { code = Ed25519Sig
            , raw = sig
            }
