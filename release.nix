with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  # Remove 'debug', as it's just an API to private implementation details
  tests = mapAttrs (_: v: removeAttrs v [ "debug" ])
                   (import ./tests.nix);

  derivationsIn = filterAttrs (_: isDerivation);
};

derivationsIn (import ./. {}) // { inherit tests; }
