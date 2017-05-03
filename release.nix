with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  these = system: import ./. {
    pkgs = import <nixpkgs> { inherit system; config = null; };
  };

  # Remove 'debug', as it's just an API to private implementation details
  tests = filterAttrs (n: _: n != "debug") (import ./tests.nix);

  derivationsIn = filterAttrs (_: isDerivation);
};

listToAttrs
  (map (system: {
         name  = system;
         value = derivationsIn (these system // { inherit tests; });
       })
       [ "i686-linux" "x86_64-linux" ])
