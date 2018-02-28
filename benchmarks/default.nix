with builtins;
with import ../pkgs.nix;
with import ../. { pkgs = stable; };
with stable.lib;
with defs.sampling;
with rec {
  python  = stable.nixpkgs1709.python.withPackages (p: [ p.subprocess32 ]);

  checks  = runner-tests ++ defs.cutoff-timer.analyse-results.checks;

  rev     = "be30d74";

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
