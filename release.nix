with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  these = system: import ./. { inherit system; };

  # Remove 'debug', as it's just an API to private implementation details
  tests = system: mapAttrs (_: v: removeAttrs v [ "debug" ])
                           (import ./tests.nix { inherit system; });

  derivationsIn = filterAttrs (_: isDerivation);
};

listToAttrs
  (map (system: {
         name  = system;
         value = derivationsIn (these system) // { tests = tests system; };
       })
       [ "i686-linux" "x86_64-linux" ])
