#!/usr/bin/env bash

# Sanity check, useful e.g. as a git pre-commit hook

# Nix is lazy, so errors may be lurking in values. Force the contents of defs to
# at least ensure our imports are syntactically correct.
nix-instantiate --show-trace --eval --read-write-mode \
                -E 'with import ./release.nix;
                    with builtins;
                    all (x: isAttrs x) (attrValues stable.defs)'
