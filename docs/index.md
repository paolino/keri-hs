# keri-hs

A learning-oriented Haskell implementation of KERI (Key Event Receipt Infrastructure), the decentralized identity protocol based on self-certifying identifiers and append-only key event logs.

## Features

- **Ed25519 cryptography** via `crypton`
- **CESR encoding/decoding** (Composable Event Streaming Representation)
- **KERI event types**: inception, rotation, interaction, receipt
- **Key state machine** with pre-rotation commitment
- **Key Event Log** management with append verification and replay
- **CLI** for key management (`init`, `sign`, `show`, `export`)
- **TCP demo** for two-party direct-mode KEL exchange

## Quick start

```bash
# Run with nix
nix run github:paolino/keri-hs#keri-cli -- init
nix run github:paolino/keri-hs#keri-cli -- show

# Or build locally
nix develop -c cabal run keri-cli -- init
```

## Scope

This is a learning-oriented implementation covering:

- Ed25519 only (no secp256k1)
- Blake2b-256 digests (no Blake3/SHA3)
- Simple integer thresholds (no fractional weights)
- Direct mode only (no witnesses, no delegation)
