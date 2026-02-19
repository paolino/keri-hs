# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i lib app test
    done
    cabal-fmt -i *.cabal
    nixfmt *.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint lib app test

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all --enable-tests -O0

# Run unit tests with optional match pattern
unit match="":
    #!/usr/bin/env bash
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit-tests -O0 \
            --test-show-details=direct
    else
        cabal test unit-tests -O0 \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Serve docs locally
docs-serve:
    #!/usr/bin/env bash
    nix develop github:paolino/dev-assets?dir=mkdocs -c mkdocs serve

# Build docs
docs-build:
    #!/usr/bin/env bash
    nix develop github:paolino/dev-assets?dir=mkdocs -c mkdocs build

# Full CI pipeline
CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    fourmolu -m check lib app test
    hlint lib app test
