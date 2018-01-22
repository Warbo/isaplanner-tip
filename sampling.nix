{ attrsToDirs, bash, fail, get-haskell-te, handleConstructors, haskellPackages,
  isabelleTypeArgs, isacosy, isacosy-theory, jq, lib, listUndefined,
  make-tebenchmark-data, make-tebenchmark-isabelle, mkBin, runCommand, stdenv,
  te-benchmark, withDeps, wrap, writeScript }:

with builtins;
with lib;
rec {
  choose_sample = { size, rep }: runCommand "choose_sample-${size}-${rep}"
    {
      REP         = toString rep;
      SIZE        = toString size;
      buildInputs = [ te-benchmark.tools ];
    }
    ''
      set -e
      choose_sample "$SIZE" "$REP" > "$out"
    '';

  runnerForSample = { size, rep }: runnerFor {
    inherit te-benchmark;
    label = "choose-${size}-${rep}";
    names = choose_sample { inherit size rep; };
  };

  # Using the same samples as haskell-te lets us directly compare Isabelle and
  # Haskell results. Outputs '{"1": {"2":["foo"], ...}, ...}' where "1" is a
  # sample size, "2" is a repetition (0, 1, 2, ...) and ["foo"] is the sample.
  samples-from-haskell-te = { filename, machine, rev }:
    with get-haskell-te rev;
    runCommand "samples-from-${filename}"
      {
        buildInputs = [ jq ];
        file    = "${haskell-te-src}/benchmarks/results/${machine}/${filename}";
        nixExpr = ''
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

  isacosy-from-sample = { label, names }:
    with rec {
      namesFile = if isList names
                     then writeScript "names-${label}"
                                      (concatStringsSep "\n" names)
                     else names;

      data = make-tebenchmark-data { te-benchmark = teb; };

      datatypes = runCommand "datatypes-${label}"
        {
          inherit data isabelleTypeArgs namesFile;
          buildInputs = [ jq ];
          pre         = "ThyConstraintParams.add_datatype' @{context} @{typ \"";
          post        = "\"}";
        }
        ''
          set -e

          # The type of each sampled name, e.g. 'nat => nat => nat' for plus
          function completeTypes {
            while read -r NAME
            do
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" || {
                echo "Couldn't get type of '$NAME'" 1>&2
                exit 1
              }
              jq -r --arg name "$NAME" '.types | .[$name]' < "$data"
            done < "$namesFile"
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

      definitions = runCommand "definitions-${label}"
        { inherit namesFile; }
        ''
          set -e
          awk '{ print "@{thms \"A."$0".simps\"}"}' < "$namesFile" |
            paste -sd ", " - > "$out"
        '';

      functions = runCommand "functions-${label}"
        {
          inherit data namesFile;
          buildInputs = [ jq ];
        }
        ''
          set -e
          set -o pipefail

          function namesTypes {
            # This assumes there are no spaces in the names, but that's a pretty
            # safe assumption given that they're Isabelle identifiers, and (if
            # they came from TIP) they'll be hex encoded as well.
            while read -r NAME
            do
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" 1>&2 || {
                echo "Name '$NAME' is needed but wasn't found in '$data'" 1>&2
                exit 1
              }
              TYPE=$(jq -r --arg name "$NAME" '.types | .[$name]' < "$data")

              # We assume the definition comes from A.thy
              echo "@{term \"A.$NAME :: $TYPE\"}" | jq -R '.'
            done < "$namesFile"
          }

          namesTypes | jq -rs 'join(", ")' > "$out"
        '';
    };
    isacosy-theory {
      inherit datatypes definitions functions;
      name = "sample-${label}";
    };

  # The attribute names (e.g. ce9c9478) are the haskell-te revisions defining
  # the code which got benchmarked. The inner 'rev' values are the haskell-te
  # revisions which contain the benchmark data (which must, of course, be
  # added in a new commit after the code which got benchmarked).
  known-samples = mapAttrs (_: args: import (samples-from-haskell-te args)) {
    ce9c9478 = {
      filename = "ce9c9478-nix-py-dirnull.json.gz";
      machine  = "desktop";
      rev      = "334d529";
    };
  };

  runnerFor = { label, names }: wrap {
    name  = "isacosy-runner-${label}";
    paths = [ bash fail isacosy ];
    vars  = {
      inherit handleConstructors;
      SHOW_RAW   = "true";
      SAMPLE     = concatStringsSep "\n" names;
      workingDir = attrsToDirs {
        "A.thy"       = tebenchmark-isabelle;
        "ISACOSY.thy" = isacosy-from-sample { inherit label names; };
      };
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail
      cd "$workingDir" || fail "Couldn't cd to '$workingDir'"

      isacosy ISACOSY.thy | "$handleConstructors"
    '';
  };

  known-runners = mapAttrs
                    (rev: mapAttrs
                            (size: mapAttrs
                                     (rep: names: runnerFor {
                                       inherit names;
                                       label = "${rev}-${size}-${rep}";
                                     })))
                    known-samples;

  runner-tests =
    with mapAttrs (n: { script, count }: runCommand "${n}-runner-test"
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
    # Try to avoid running multiple isacosy processes in parallel, since they're
    # resource-hungry and include a timeout. We make nat run first, since it's
    # monomorphic and therefore more 'basic' than the polymorphic list test.
    withDeps [ nat ] list;
}
