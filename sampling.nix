{ get-haskell-te, jq, lib, runCommand }:

with lib;
rec {
  choose_sample = { size, rep }: runCommand "choose_sample-${size}-${rep}"
  {}
  ''
    FIXME: choose_sample > "$out"
  '';

  # Using the same samples as haskell-te lets us directly compare Isabelle and
  # Haskell results. Outputs '{"1": {"2":["foo"], ...}, ...}' where "1" is a
  # sample size, "2" is a repetition (0, 1, 2, ...) and ["foo"] is the sample.
  samples-from-haskell-te = { filename, machine, rev, sha256 }:
    with rec {
      src = get-haskell-te { inherit rev sha256; };
    };
    runCommand "samples-from-${filename}"
      {
        buildInputs = [ jq ];
        file        = "${src}/benchmarks/results/${machine}/${filename}";
        nixExpr     = ''
          with builtins;
          fromJSON (readFile ./samples.json)
        '';
      }
      ''
        set -e
        mkdir "$out"
        gunzip < "$file"                       |
          jq '.results                         |
              ."quickspectip.track_data"       |
              .result                          |
              .[0]                             |
              map_values(.reps                 |
                         map_values(.sample))' > "$out/samples.json"
        echo "$nixExpr" > "$out/default.nix"
      '';

  known_samples = mapAttrs (_: args: import (samples-from-haskell-te args)) {
    ce9c9478 = {
      filename = "ce9c9478-nix-py-dirnull.json.gz";
      machine  = "desktop";
      rev      = "334d529";
      sha256   = "109g8hkpggjjlw7ksd7l157jknp4wkg9lbjlyiqqvqzah2kl65jf";
    };
  };
}
