with builtins;
with (import <nixpkgs> {}).lib;
with rec {
  # Remove 'helpers', as it's just an API to private implementation details
  trimTests = v: removeAttrs v [ "helpers" "defs" ];

  # Don't try to build functions, etc.
  derivationsIn = filterAttrs (_: isDerivation);

  # Collect up definitions and tests
  fromPkgs = pkgs:
    with { withTests = import ./tests.nix pkgs; };
    derivationsIn withTests.defs // { tests = trimTests withTests.tests; };
};

mapAttrs (_: fromPkgs) (import ./pkgs.nix)
