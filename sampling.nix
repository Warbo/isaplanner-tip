{ attrsToDirs, bash, fail, gawk, get-haskell-te, handleConstructors,
  haskellPackages, isabelleTypeArgs, isacosy, isacosy-theory, jq, lib,
  listUndefined, make-tebenchmark-data, make-tebenchmark-isabelle, mkBin,
  nothing, runCommand, stdenv, te-benchmark, withDeps, wrap, writeScript }:

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

  dataForSample = { label, names, teb ? te-benchmark }: rec {
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

    data    = make-tebenchmark-data     { te-benchmark = teb; };

    theory  = make-tebenchmark-isabelle { te-benchmark = teb; };

    allDefs = runCommand "all-defs"
      {
        inherit theory;
        buildInputs = [ gawk jq ];
      }
      ''
        set -e

        # We have three conflicting criteria for listing definitions of
        # sampled names:
        #
        #  - Going from a sample to their definitions needs to be reasonably
        #    fast, since it will be executed a lot.
        #  - We must output definitions in the same order as the names appear
        #    in the sample, because they must match up with the 'functions'
        #    list for zipping.
        #  - Defs which use 'definition' will differ from those which use
        #    'function'.
        #
        # We satisfy these by creating all defs once, up front, here. The name
        # to definition process can then just traverse the names as given, and
        # look up the definitions.

        function getByKeyword {
          # Awk command from https://unix.stackexchange.com/a/205854/63735
          < "$theory" tr  '\n' ' '         | grep -o " $1 [^:]*" |
                      sed -e "s/ $1 /\n/g" | awk '{$1=$1};1'     | grep '^.'
        }

        # All 'definitions' follow the keyword 'definition'
        getByKeyword definition > defs

        # Functions may follow 'function' or 'and' (for mutual-recursion)
        getByKeyword function >  functions
        echo ""               >> functions
        getByKeyword 'and'    >> functions

        grep '^.' < functions > funs

        jq -R '{(.) : ("@{thms \"A." + . + "_def\"}")}' < defs |
        jq -s 'reduce .[] as $obj ({}; . + $obj)'       > defs.json

        jq -R '{(.) : ("@{thms \"A." + . + ".simps\"}")}' < funs |
        jq -s 'reduce .[] as $obj ({}; . + $obj)'       > funs.json

        jq -n --argfile d defs.json --argfile f funs.json '$d + $f' > "$out"
      '';

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
            # Check we have a type for this name
            jq -e --arg name "$NAME" '.types | has($name)' < "$data" \
                                                           > /dev/null ||
              fail "Couldn't get type of '$NAME'"

            # Get this name's type
            jq --arg name "$NAME" '.types | .[$name]' < "$data"
          done < "$namesFile" | jq -s '.'
        }

        completeTypes | "$isabelleTypeArgs"          |
                        jq -r '.[]'                  |
                        grep -i 'global[0-9a-fA-F]*' |
                        sort -u                      |
                        while read -r T
        do
          echo "|> ThyConstraintParams.add_datatype' @{context} @{typ \"$T\"}"
        done > "$out"
      '';

    definitions = runCommand "definitions-${label}"
      {
        inherit allDefs namesFile theory;
        buildInputs = [ jq ];
      }
      ''
        set -e

        function defs {
          # Wrap up each sampled name in quotes
          jq -R '.' < "$namesFile"
        }

        # Slurp each name into a list, look them up in allDefs, then join
        defs | jq -rs --argfile defs "$allDefs" \
                  'map($defs[.]) | join(", ")'  > "$out"
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

  isacosy-from-sample-untested = args: isacosy-theory {
    inherit (dataForSample args) datatypes definitions functions undefined;
    name = "sample-${args.label}";
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
    with rec {
      checkConjectures = { count, name, script }: runCommand
        "${name}-runner-test"
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
        '';

      # Contains plus, times and exp for nats, which
      # should be easy for IsaCoSy to find conjectures
      # for. The fact that the indices approximate pi is
      # purely a coincidence!
      nat = {
        count  = "";  # Find at least one conjecture
        name   = "nat";
        script = known-runners.ce9c9478."3"."14";
      };

      # Contains list reverse, which we can use to test
      # parameterised types.
      list = {
        # We should only find 'rev (rev x) ~= x', since
        # anything else requires constructors, which we
        # should be stripping out
        count  = 1;
        name = "list";
        script = known-runners.ce9c9478."1"."22";
      };

      functionDefsMatchUp = runCommand "function-defs-match-up"
        {
          inherit (dataForSample {
            label = "test";
            names = choose_sample { size = "200"; rep = "0"; };
          }) definitions functions;
          buildInputs = [ fail ];
        }
        ''
          set -e
          echo "Checking '$functions' matches up with '$definitions'" 1>&2

          tr ',' '\n' < "$definitions" | grep -o 'A\.[^: ._]*' > defs
          tr ',' '\n' < "$functions"   | grep -o 'A\.[^: ._]*' > funs

          while read -r PAIR
          do
            DEF=$(echo "$PAIR" | cut -f1)
            FUN=$(echo "$PAIR" | cut -f2)

            # We stop at the first mismatched pair, since an extra/missing name
            # would cause all subsequent pairs to mismatch.
            [[ "x$DEF" = "x$FUN" ]] ||
              fail "Mismatched names: function '$FUN', definition '$DEF'"
          done < <(paste defs funs)

          mkdir "$out"
        '';

      tests = [ functionDefsMatchUp ]; #[ (checkConjectures nat) (checkConjectures list) ];
    };
    withDeps tests nothing;
}
