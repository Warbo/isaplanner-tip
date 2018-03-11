with builtins;
with import ../pkgs.nix;
with import ../. { pkgs = stable; };
with stable.lib;
with defs.sampling;
with rec {
  python  = stable.nixpkgs1709.python.withPackages (p: [ p.subprocess32 ]);

  checks  = runner-tests ++ defs.cutoff-timer.analyse-results.checks;

  # We originally ran only the even sizes (2, 4, 6, ..., 20), but turned out to
  # have enough time to run the odds too
  #rev    = "be30d74";  # Even numbered sample sizes
  #rev    = "3c15e23";  # Odd numbered sample sizes
  rev     = "1c6ae74";  # Rep 30

  runners = mapAttrs (size: mapAttrs (rep: runner: rec {
                       inherit runner;
                       sample   = attrByPath [ rev size rep ]
                                             (abort (toJSON {
                                               inherit rev size rep;
                                               error = "No such sample";
                                             }))
                                             known-samples;
                       analyser = sampleAnalyser {
                         REP           = rep;
                         SIZE          = size;
                         SAMPLED_NAMES = concatStringsSep "\n" sample;
                       };
                     }))
                     (getAttr rev known-runners);

  wrapper = stable.mkBin {
    name  = "python";
    file  = "${python}/bin/python";
    paths = [ python ];
    vars  = {
      runners      = stable.writeScript "runners.json" (toJSON runners);
      timeout_secs = "300";
    };
  };
};
stable.withDeps checks wrapper
