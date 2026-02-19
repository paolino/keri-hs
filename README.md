# keri-hs

A learning-oriented Haskell implementation of [KERI](https://keri.one/)
(Key Event Receipt Infrastructure).

**[Documentation](https://paolino.github.io/keri-hs/)**

## Scope

- Ed25519 only (no secp256k1)
- Blake2b-256 digest (Blake3 architecture, crypton backend)
- Simple integer thresholds (no fractional weights)
- Direct mode only (no witnesses, no delegation)

## Building

```bash
nix develop -c just build
```

## Testing

```bash
nix develop -c just unit
```

## CLI

```bash
nix run .#keri-cli -- init
nix run .#keri-cli -- show
nix run .#keri-cli -- sign "hello"
nix run .#keri-cli -- export
```

## Direct Mode Demo

```bash
# Terminal 1
nix develop -c cabal run keri-demo -- serve 9000

# Terminal 2
nix develop -c cabal run keri-demo -- connect localhost 9000
```

## License

Apache-2.0
