{ pkgs ? (import ./pkgs.nix).stable }:

with builtins;
with { pkgsAlias = pkgs; };  # Since 'with pkgs' shadows the name 'pkgs'
with pkgs;

rec {
  inherit (defs.isacosy) isacosy isacosy-nat-eqs;

  defs = {
    haskell-te = callPackage ./haskell-te.nix {};
    isaplanner = callPackage ./isaplanner.nix {};
    isacosy    = callPackage ./isacosy.nix {
      inherit (defs.isaplanner) isaplanner;
      inherit (defs.tebenchmark-isabelle) te-benchmark tebenchmark-data;
    };
    sampling = callPackage ./sampling.nix {
      inherit (defs.haskell-te) get-haskell-te;
      inherit (defs.isacosy) isacosy isacosy-theory;
      inherit (defs.tebenchmark-isabelle) tebenchmark-data tebenchmark-isabelle;
    };
    tebenchmark-isabelle = callPackage ./tebenchmark-isabelle.nix {
      inherit (defs.isaplanner) isaplanner;
    };
  };
}
