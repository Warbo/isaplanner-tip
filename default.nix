{ pkgs ? (import ./pkgs.nix).stable }:

with builtins;
with { pkgsAlias = pkgs; };  # Since 'with pkgs' shadows the name 'pkgs'
with pkgs;

rec {
  inherit (defs.isacosy) isacosy;

  defs = {
    cutoff-timer = callPackage ./cutoff-timer.nix {
      inherit (defs.scripts ) cutoff-timer;
      inherit (defs.sampling) runnerForSample sampleAnalyser;
    };
    haskell-te = callPackage ./haskell-te.nix {};
    isaplanner = callPackage ./isaplanner.nix {};
    isacosy    = callPackage ./isacosy.nix {
      inherit (defs.isaplanner          ) isaplanner;
      inherit (defs.scripts             ) eqsToJson extractEqs;
      inherit (defs.tebenchmark-isabelle) te-benchmark;
    };
    sampling = callPackage ./sampling.nix {
      inherit (defs.haskell-te          ) get-haskell-te;
      inherit (defs.isacosy             ) isacosy isacosy-theory;
      inherit (defs.scripts             ) isabelleTypeArgs;
      inherit (defs.tebenchmark-isabelle) find-undefined-cases
                                          handleConstructors
                                          make-tebenchmark-data
                                          make-tebenchmark-isabelle
                                          te-benchmark;
    };
    scripts              = callPackage ./scripts {};
    tebenchmark-isabelle = callPackage ./tebenchmark-isabelle.nix {
      inherit (defs.isaplanner) isaplanner;
      inherit (defs.scripts   ) getBenchmarkTypes getPreprocessed
                                nonExhaustiveScraper
                                stripConstructorsDestructors;
    };
  };
}
