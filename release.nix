with builtins;
with import <nixpkgs> {};
with lib;
with rec {
  # Imports a Nix file, overriding currentSystem to be the given system
  forSys = system:
    let overrides = {
          # Return the given system as if it were the current one
          currentSystem = system;

          # Force nested imports to use the given system too
          import       = scopedImport overrides;
          scopedImport = given: scopedImport (overrides // given);

          # Update builtins.currentSystem (and builtins.builtins) too
          builtins = builtins // overrides;
        };
     in scopedImport overrides;

  # Remove 'debug', as it's just an API to private implementation details
  tests = system: mapAttrs (_: v: removeAttrs v [ "debug" ])
                           (forSys system ./tests.nix);

  derivationsIn = filterAttrs (_: isDerivation);

  defs = system: derivationsIn (forSys system ./. {});
};

genAttrs [ "i686-linux" "x86_64-linux" ]
         (system: defs system // { tests = tests system; })
