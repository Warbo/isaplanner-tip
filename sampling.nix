{ bash, get-haskell-te, isacosy, isacosy-theory, jq, lib, runCommand,
  tebenchmark-data, wrap, writeScript }:

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

  isacosy-from-sample = { names, rep, size }:
    with rec {
      functions = runCommand "functions-${size}-${rep}"
        {
          buildInputs = [ jq ];
          data        = tebenchmark-data;
        }
        ''
          set -e
          set -o pipefail

          function namesTypes {
            # This assumes there are no spaces in the names, but that's a pretty
            # safe assumption given that they're Isabelle identifiers, and (if
            # they came from TIP) they'll be hex encoded as well.
            for NAME in ${concatStringsSep " " names}
            do
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" 1>&2 || {
                echo "Name '$NAME' is needed but wasn't found in '$data'" 1>&2
                exit 1
              }
              TYPE=$(jq -r --arg name "$NAME" '.types | .[$name]' < "$data")

              # We assume the definition comes from A.thy
              echo "@{term \"A.$NAME :: $TYPE\"}" | jq -R '.'
            done
          }

          namesTypes | jq -rs 'join(", ")' > "$out"
        '';
    };
    isacosy-theory {
      inherit functions;
      name        = "sample-${size}-${rep}";
      datatypes   = writeScript "fixme" "";
      definitions = writeScript "fixme" "";
      undefined   = writeScript "fixme" "";
    };

  known-samples = mapAttrs (_: args: import (samples-from-haskell-te args)) {
    ce9c9478 = {
      filename = "ce9c9478-nix-py-dirnull.json.gz";
      machine  = "desktop";
      rev      = "334d529";
      sha256   = "109g8hkpggjjlw7ksd7l157jknp4wkg9lbjlyiqqvqzah2kl65jf";
    };
  };
}
