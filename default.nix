{ pkgs ? (import ./pkgs.nix).stable }:

with builtins;
with { pkgsAlias = pkgs; };  # Since 'with pkgs' shadows the name 'pkgs'
with pkgs;

rec {
  inherit (defs.isaplanner)           isaplanner;
  inherit (defs.tebenchmark-isabelle) te-benchmark tebenchmark-isabelle;
  inherit (defs.isacosy)              isacosy;

  defs = {
    haskell-te           = callPackage ./haskell-te.nix {};
    isaplanner           = callPackage ./isaplanner.nix {};
    tebenchmark-isabelle = callPackage ./tebenchmark-isabelle.nix {
      inherit isaplanner;
    };
    isacosy              = callPackage ./isacosy.nix {
      inherit isaplanner te-benchmark tebenchmark-isabelle;
    };
    sampling             = callPackage ./sampling.nix {
      inherit (defs.haskell-te) get-haskell-te;
    };
  };

  hackage = h: n: h.callPackage (runCabal2nix { url = "cabal://${n}"; });

  isacosy-nat-eqs = stdenv.mkDerivation {
    name = "isacosy-nat-eqs";
    data = isacosy-nat;

    buildCommand = ''
      source $stdenv/setup

      set -e

      [[ -f "$data/output" ]] || {
        echo "Error: Isabelle output '$data/output' doesn't exist" 1>&2
        exit 1
      }

      mkdir -p "$out"
      "${./extract_eqs.sh}" < "$data/output" > "$out/equations"
    '';
  };
}
