module Keri.KeyState
    ( KeyState (..)
    , initialState
    , applyEvent
    ) where

{- |
Module      : Keri.KeyState
Description : Key state machine
Copyright   : (c) 2026 Cardano Foundation
License     : Apache-2.0

Maintains the key state derived from processing KERI
events. The state tracks current keys, next-key
commitments, thresholds, and the last event digest.
-}

import Data.Text (Text)
import Keri.Event
    ( Event (..)
    , InceptionData (..)
    , InteractionData (..)
    , RotationData (..)
    )
import Keri.KeyState.PreRotation (verifyCommitment)

-- | Current key state of an identifier.
data KeyState = KeyState
    { statePrefix :: Text
    , stateSequenceNumber :: Int
    , stateLastDigest :: Text
    , stateSigningThreshold :: Int
    , stateKeys :: [Text]
    , stateNextThreshold :: Int
    , stateNextKeys :: [Text]
    , stateWitnessThreshold :: Int
    , stateWitnesses :: [Text]
    , stateConfig :: [Text]
    }
    deriving stock (Show, Eq)

-- | Derive initial key state from an inception event.
initialState :: InceptionData -> KeyState
initialState d =
    KeyState
        { statePrefix = prefix d
        , stateSequenceNumber =
            sequenceNumber d
        , stateLastDigest = digest d
        , stateSigningThreshold =
            signingThreshold d
        , stateKeys = keys d
        , stateNextThreshold = nextThreshold d
        , stateNextKeys = nextKeys d
        , stateWitnessThreshold =
            witnessThreshold d
        , stateWitnesses = witnesses d
        , stateConfig = config d
        }

{- | Apply an event to the current key state,
producing the new state or an error.
-}
applyEvent
    :: KeyState -> Event -> Either String KeyState
applyEvent ks = \case
    Inception _ ->
        Left
            "Cannot apply inception to existing state"
    Rotation d -> applyRotation ks d
    Interaction d -> applyInteraction ks d
    Receipt _ -> Right ks

applyRotation
    :: KeyState
    -> RotationData
    -> Either String KeyState
applyRotation ks d = do
    checkPrefix ks (prefix d)
    checkSequence ks (sequenceNumber d)
    checkPrior ks (priorDigest d)
    verifyPreRotation
        (stateNextKeys ks)
        (keys d)
    Right
        ks
            { stateSequenceNumber =
                sequenceNumber d
            , stateLastDigest = digest d
            , stateSigningThreshold =
                signingThreshold d
            , stateKeys = keys d
            , stateNextThreshold =
                nextThreshold d
            , stateNextKeys = nextKeys d
            }

applyInteraction
    :: KeyState
    -> InteractionData
    -> Either String KeyState
applyInteraction ks d = do
    checkPrefix ks (prefix d)
    checkSequence ks (sequenceNumber d)
    checkPrior ks (priorDigest d)
    Right
        ks
            { stateSequenceNumber =
                sequenceNumber d
            , stateLastDigest = digest d
            }

checkPrefix :: KeyState -> Text -> Either String ()
checkPrefix ks p
    | statePrefix ks /= p =
        Left "Prefix mismatch"
    | otherwise = Right ()

checkSequence
    :: KeyState -> Int -> Either String ()
checkSequence ks sn
    | sn /= stateSequenceNumber ks + 1 =
        Left $
            "Expected sequence "
                <> show (stateSequenceNumber ks + 1)
                <> ", got "
                <> show sn
    | otherwise = Right ()

checkPrior :: KeyState -> Text -> Either String ()
checkPrior ks p
    | stateLastDigest ks /= p =
        Left "Prior digest mismatch"
    | otherwise = Right ()

verifyPreRotation
    :: [Text] -> [Text] -> Either String ()
verifyPreRotation commitments newKeys
    | length commitments /= length newKeys =
        Left "Key count mismatch with commitments"
    | otherwise =
        mapM_ checkOne (zip newKeys commitments)
  where
    checkOne (key, commitment) = do
        ok <- verifyCommitment key commitment
        if ok
            then Right ()
            else
                Left
                    "Pre-rotation commitment mismatch"
