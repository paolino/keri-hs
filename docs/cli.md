# CLI Reference

The `keri-cli` executable provides key management for KERI identifiers.

## Installation

```bash
nix run github:paolino/keri-hs#keri-cli -- <command>
```

Or build from source:

```bash
nix develop -c cabal build keri-cli
```

## Commands

### `init`

Create a new KERI identifier with an Ed25519 key pair.

```bash
keri-cli init
# Initialized: EaBc...XyZ
```

This generates:

- A signing key pair (current)
- A pre-rotated next key pair
- An inception event with SAID as the self-addressing identifier

Storage layout:

```
~/.keri/
  current           # text file with current prefix
  <prefix>/
    kel.ndjson      # newline-delimited event JSON
    secret.key      # base64url-encoded Ed25519 secret key
```

### `sign`

Sign a message with the current identifier's key.

```bash
keri-cli sign "hello world"
# 0BAe...signature...
```

The output is a CESR-encoded Ed25519 signature (code `0B`, 88 characters).

### `show`

Display the current identifier state.

```bash
keri-cli show
# Events: 1
# Prefix: EaBc...XyZ
# Last digest: EaBc...XyZ
```

Shows the number of events in the KEL, the identifier prefix, and the most recent event digest.

### `export`

Export the full Key Event Log as newline-delimited JSON.

```bash
keri-cli export
# {"v":"KERI10JSON0001ab_","t":"icp","d":"EaBc...","i":"EaBc...", ...}
```

Each line is a serialized KERI event in canonical JSON format with deterministic field ordering.

## Storage

All data is stored under `~/.keri/`:

| File | Format | Contents |
|------|--------|----------|
| `current` | UTF-8 text | Active identifier prefix |
| `<prefix>/kel.ndjson` | NDJSON | Key Event Log |
| `<prefix>/secret.key` | Base64url | Ed25519 secret key (32 bytes) |

## CESR output format

Signatures and identifiers use CESR (Composable Event Streaming Representation) encoding:

| Code | Type | Length |
|------|------|--------|
| `D` | Ed25519 public key | 44 chars |
| `F` | Blake2b-256 digest | 44 chars |
| `0B` | Ed25519 signature | 88 chars |
