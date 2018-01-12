with import ../pkgs.nix;
with import ../. { pkgs = stable; };
with defs.sampling;
with rec {
  python = stable.python.withPackages (pyPkgs: [ pyPkgs.subprocess32 ]);

  rev     = "ce9c9478";

  runners = mapAttrs (size: mapAttrs (rep: runner: rec {
                       inherit runner;
                       sample   = attrByPath [ rev size rep ] known-samples;
                       analyser = sampleAnalyser {
                         REP        = rep;
                         SIZE       = size;
                         # TODO: haskell-te commit e911a65 adds a SAMPLED_NAMES
                         # argument, which lets us pass in our list of strings
                         # without writing them to a file
                         sampleFile = writeScript "sample-${rev}-${size}-${rep}"
                                        (concatStringsSep "\n" sample);
                       };
                     }))
                     (getAttr rev known-runners);

  sampleAnalyser = stable.callPackage
    "${defs.haskell-te.haskell-te-src}/benchmarks/sampleAnalyser.nix"
    { inherit (defs.haskell-te.haskell-te) analysis; };

  wrapper = stable.mkBin {
    name  = "python";
    paths = [ python ];
    vars  = {
      inherit runners;
      timeout_secs = "180";
    };
    file  = "${python}/bin/python";
  };
};
stable.withDeps [ runner-tests ] python
