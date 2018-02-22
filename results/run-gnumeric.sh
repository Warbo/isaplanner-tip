#!/usr/bin/env bash

P='with rec {
     sysPkgs    = import <nixpkgs> { config = {}; };
     pinnedSrc  = sysPkgs.fetchFromGitHub {
       owner  = "NixOS";
       repo   = "nixpkgs";
       rev    = "39cd40f"; # 17.09
       sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
     };
     pinnedPkgs = import pinnedSrc { config = {}; };
   };
   pinnedPkgs.gnumeric'

BASE=$(dirname "$(readlink -f "$0")")
FILE="$BASE/cutoff-times-vis.gnumeric"
export FILE
CMD='gnumeric "$FILE"'

[[ -f "$FILE" ]] || {
    echo "File '$FILE' not found, running gnumeric on it's own" 1>&2
    CMD='gnumeric'
}

nix-shell -p "$P" --run "$CMD"
