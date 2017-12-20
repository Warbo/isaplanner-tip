{ attrsToDirs, bash, fail, get-haskell-te, handleConstructors, haskellPackages,
  isabelleTypeArgs, isacosy, isacosy-theory, jq, lib, mkBin, runCommand,
  tebenchmark-data, tebenchmark-isabelle, withDeps, wrap, writeScript }:

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
      datatypes = runCommand "datatypes-${size}-${rep}"
        {
          inherit isabelleTypeArgs;
          buildInputs = [ jq ];
          data        = tebenchmark-data;
          pre         = "ThyConstraintParams.add_datatype' @{context} @{typ \"";
          post        = "\"}";

        }
        ''
          set -e

          # The type of each sampled name, e.g. 'nat => nat => nat' for plus
          function completeTypes {
            for NAME in ${concatStringsSep " " names}
            do
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" || {
                echo "Couldn't get type of '$NAME'" 1>&2
                exit 1
              }
              jq -r --arg name "$NAME" '.types | .[$name]' < "$data"
            done
          }

          # Output arguments of a function type e.g. 'a => b => c' gives a and b
          function allArgs {
            completeTypes | while read -r TYPE
            do
              echo "$TYPE" | "$isabelleTypeArgs"
            done | sort -u
          }

          allArgs | while read -r ARG
          do
            echo "|> ThyConstraintParams.add_datatype' @{context} @{typ \"$ARG\"}"
          done > "$out"
        '';

      definitions = writeScript "definitions-${size}-${rep}"
        (concatStringsSep ", " (map (name: ''@{thms "A.${name}.simps"}'')
                                    names));

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
      inherit datatypes definitions functions;
      name = "sample-${size}-${rep}";
    };

  known-samples = mapAttrs (_: args: import (samples-from-haskell-te args)) {
    ce9c9478 = {
      filename = "ce9c9478-nix-py-dirnull.json.gz";
      machine  = "desktop";
      rev      = "334d529";
      sha256   = "109g8hkpggjjlw7ksd7l157jknp4wkg9lbjlyiqqvqzah2kl65jf";
    };
  };

  known-theories = mapAttrs (_: mapAttrs (size: mapAttrs (rep: names:
                              isacosy-from-sample { inherit names size rep; })))
                            known-samples;

  known-runners = mapAttrs (rev: mapAttrs (size: mapAttrs (rep: theory:
                             wrap {
                               name   = "isacosy-runner-${rev}-${size}-${rep}";
                               paths  = [ bash fail isacosy ];
                               vars   = {
                                 inherit handleConstructors;
                                 SAMPLE     = concatStringsSep "\n" (attrByPath
                                                [ rev size rep ]
                                                (abort (toJSON {
                                                  inherit rep rev size;
                                                  err = "Sample not found";
                                                }))
                                                known-samples);
                                 workingDir = attrsToDirs {
                                   "A.thy"       = tebenchmark-isabelle;
                                   "ISACOSY.thy" = theory;
                                 };
                               };
                               script = ''
                                 #!/usr/bin/env bash
                                 set -e
                                 set -o pipefail
                                 cd "$workingDir" ||
                                   fail "Couldn't cd to '$workingDir'"

                                 isacosy ISACOSY.thy | "$handleConstructors"
                               '';
                             })))
                           known-theories;

  runner-tests = mapAttrs (n: { script, count }: runCommand "${n}-runner-test"
                            {
                              inherit n script;
                              c           = toString count;
                              buildInputs = [ fail jq ];
                            }
                            ''
                              set -e
                              echo "Exploring $n theory" 1>&2
                              OUTPUT=$("$script" | tee >(cat 1>&2))

                              if [[ -n "$c" ]]
                              then
                                echo "$OUTPUT" |
                                  jq -e 'length | tostring | . == env["c"]' ||
                                    fail "Should have found $c conjectures"
                              else
                                echo "$OUTPUT" | jq -e 'length | . >0 ' ||
                                  fail "No conjectures found"
                              fi
                              mkdir "$out"
                            '')
                          {
                            # Contains plus, times and exp for nats, which
                            # should be easy for IsaCoSy to find conjectures
                            # for. The fact that the indices approximate pi is
                            # purely a coincidence!
                            nat = {
                              count  = "";  # Find at least one conjecture
                              script = known-runners.ce9c9478."3"."14";
                            };

                            # Contains list reverse, which we can use to test
                            # parameterised types.
                            list = {
                              # We should only find 'rev (rev x) ~= x', since
                              # anything else requires constructors, which we
                              # should be stripping out
                              count  = 1;
                              script = known-runners.ce9c9478."1"."22";
                            };
                          };
}
