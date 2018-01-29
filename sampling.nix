{ attrsToDirs, bash, fail, get-haskell-te, handleConstructors, haskellPackages,
  isabelleTypeArgs, isacosy, isacosy-theory, jq, lib, listUndefined,
  make-tebenchmark-data, make-tebenchmark-isabelle, mkBin, python, runCommand,
  stdenv, te-benchmark, withDeps, wrap, writeScript }:

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

  isacosy-from-sample =
    with {
      filter-check = runCommand "filter-test"
        {
          buildInputs = [ jq ];

          theory = isacosy-from-sample-untested {
            label = "filter-test";
            names = choose_sample { size = "200"; rep = "0"; };
          };

          fixes = ./scripts/fixes.json;
        }
        ''
          set -e

          while read -r TERM
          do
            if grep -F "$TERM" < "$theory"
            then
              fail "Should have stripped '$TERM' from theory '$theory'"
            fi
          done < <(jq -r '[.nontypes, .dependents] | .[] |
                                          .encoded | .[]' < "$fixes")

          mkdir "$out"
        '';
    };
    args: withDeps [ filter-check ] (isacosy-from-sample-untested args);

  isacosy-from-sample-untested = { label, names, teb ? te-benchmark }:
    with rec {
      namesFile = runCommand "filtered-names-${label}"
        {
          source = if isList names
                      then writeScript "names-${label}"
                                       (concatStringsSep "\n" names)
                      else names;
          buildInputs = [ jq ];
          fixes       = ./scripts/fixes.json;
        }
        ''
          set -e
          cp "$source" tmp

          while read -r NAME
          do
            grep -v -x -F "$NAME" < tmp > tmp2
            mv tmp2 tmp
          done < <(jq -r '(.dependents.encoded +
                           .nontypes.encoded) | .[]' < "$fixes")

          mv tmp "$out"
        '';

      data = make-tebenchmark-data { te-benchmark = teb; };

      theory = make-tebenchmark-isabelle { te-benchmark = teb; };

      datatypes = runCommand "datatypes-${label}"
        {
          inherit data isabelleTypeArgs namesFile;
          buildInputs = [ fail jq ];
          pre         = "ThyConstraintParams.add_datatype' @{context} @{typ \"";
          post        = "\"}";
        }
        ''
          set -e

          # The type of each sampled name, e.g. 'nat => nat => nat' for plus
          function completeTypes {
            while read -r NAME
            do
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" \
                                                             > /dev/null ||
                fail "Couldn't get type of '$NAME'"

              jq --arg name "$NAME" '.types | .[$name]' < "$data"
            done < "$namesFile" | jq -s '.'
          }

          # Output arguments of a function type e.g. 'a => b => c' gives a and b
          # We grep for 'global' to avoid standalone parameters like "'local1",
          # e.g. from "cons :: 'local1 => 'local1 list => 'local1 list".
          function allArgs {
            completeTypes | "$isabelleTypeArgs" | jq -r '.[]' | grep -i global |
                            sort -u
          }

          allArgs | while read -r ARG
          do
            echo "|> ThyConstraintParams.add_datatype' @{context} @{typ \"$ARG\"}"
          done > "$out"
        '';

      definitions = runCommand "definitions-${label}"
        {
          inherit namesFile python theory;
          commaSep = wrap {
            name   = "commaSep";
            paths  = [ python ];
            script = ''
              #!/usr/bin/env python
              import sys
              lines    = sys.stdin.readlines()
              nonempty = [l.strip() for l in lines if l.strip() != ""]
              print(', '.join(nonempty))
            '';
          };
        }
        ''
          set -e

          function trimSpaces {
            # From https://unix.stackexchange.com/a/205854/63735
            awk '{$1=$1};1'
          }

          DEFS=$(grep -o '^definition[^:]*' < "$theory" |
                 sed -e 's/^definition//g' | trimSpaces)

          FUNS=$(grep -o '^function[^:]*' < "$theory" |
                 sed -e 's/^function//g' | trimSpaces)

          function onlyDefs {
            grep -x -F -f <(echo "$DEFS") < "$namesFile"
          }

          function onlyFuns {
            grep -x -F -f <(echo "$FUNS") < "$namesFile"
          }

          function isaDefs {
            # Suffix each "definition" with '_def'
            onlyDefs | awk '{printf("\n\n@{thms \"A.%s_def\"}\n\n", $0)}'
          }

          function isaFunctions {
            # Suffix each "function" with '.simps'
            onlyFuns | awk '{printf("\n\n@{thms \"A.%s.simps\"}\n\n", $0)}'
          }

          function isaAll {
            isaDefs
            isaFunctions
          }

          # Process each name on a separate line then combine with commas
          isaAll | "$commaSep" > "$out"
        '';

      functions = runCommand "functions-${label}"
        {
          inherit data namesFile;
          buildInputs = [ fail jq ];
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
              jq -e --arg name "$NAME" '.types | has($name)' < "$data" \
                                                             > /dev/null ||
                fail "Name '$NAME' is needed but wasn't found in '$data'"

              TYPE=$(jq -r --arg name "$NAME" '.types | .[$name]' < "$data")

              # We assume the definition comes from A.thy
              echo "@{term \"A.$NAME :: $TYPE\"}" | jq -R '.'
            done < "$namesFile"
          }

          namesTypes | jq -rs 'join(", ")' > "$out"
        '';

      undefined = stdenv.mkDerivation {
        inherit namesFile;
        name    = "undefined-${label}";
        builder = listUndefined { te-benchmark = teb; };
      };
    };
    isacosy-theory {
      inherit datatypes definitions functions undefined;
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

  runnerFor = { label, names, te-benchmark }: wrap {
    name  = "isacosy-runner-${label}";
    paths = [ bash fail isacosy ];
    vars  = {
      inherit handleConstructors;
      SHOW_RAW   = "true";

      workingDir = attrsToDirs {
        "A.thy"       = make-tebenchmark-isabelle { inherit te-benchmark; };
        "ISACOSY.thy" = isacosy-from-sample {
          inherit label names;
          teb = te-benchmark;
        };
      };

      sampleFile = if isList names
                      then writeScript "sampleFile"
                                       (concatStringsSep "\n" names)
                      else names;
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail
      cd "$workingDir" || fail "Couldn't cd to '$workingDir'"

      SAMPLE=$(cat "$sampleFile")
      export SAMPLE

      isacosy ISACOSY.thy | "$handleConstructors"
    '';
  };

  known-runners =
    mapAttrs
      (rev: mapAttrs
              (size: mapAttrs
                       (rep: names: runnerFor {
                         inherit names;
                         label        = "${rev}-${size}-${rep}";
                         te-benchmark =
                           with get-haskell-te rev;
                           haskell-te.tipBenchmarks;
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
                          jq -e 'length | tostring | . == env["c"]' || {
                            echo -e "OUTPUT:\n\n$OUTPUT\n\nEND OUTPUT" 1>&2
                            fail "Should have found $c conjectures"
                        }
                      else
                        echo "$OUTPUT" | jq -e 'length | . > 0' || {
                          echo -e "OUTPUT:\n\n$OUTPUT\n\nEND OUTPUT" 1>&2
                          fail "No conjectures found"
                        }
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
