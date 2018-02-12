{ attrsToDirs, bash, cutoff-timer, fail, find-undefined-cases, gawk,
  get-haskell-te, handleConstructors, haskellPackages, isabelleTypeArgs,
  isacosy, isacosy-theory, jq, lib, make-tebenchmark-data,
  make-tebenchmark-isabelle, mkBin, nothing, runCommand, stdenv, te-benchmark,
  withDeps, wrap, writeScript }:

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

  # Since we're running on a lot of samples, we need a timeout to cut off
  # execution. We figure that it shouldn't be more than an hour, but we might be
  # able to get away with less, if the majority of runs finish below a certain
  # time. These samples let us check if such a time exists.
  find-cutoff-time =
    with rec {
      mkRunners = sizes: reps:
        genAttrs (map toString sizes)
                 (size: genAttrs (map toString (range 0 (reps - 1)))
                                 (rep: runnerForSample {
                                   inherit rep size;
                                 }));

      times-out = runCommand "cutoff-timer-times-out"
        {
          buildInputs = [ fail jq ];
          cmd = cutoff-timer {
            runners      = mkRunners [1 2 3] 2;
            timeout_secs = 2;
          };
        }
        ''
          BEFORE=$(date "+%s")
          "$cmd" > /dev/null
          AFTER=$(date +%s)

          DIFF=$(( AFTER - BEFORE ))
          [[ "$DIFF" -lt 20 ]] ||
            fail "Should've taken 12 seconds, took '$DIFF'"

          mkdir "$out"
        '';

      have-output = runCommand "cutoff-time-has-output"
        {
          buildInputs = [ fail jq ];
          cmd = cutoff-timer {
            runners      = mkRunners [1 2 3] 2;
            timeout_secs = 2;
          };
        }
        ''
          "$cmd" > output

          function q {
            jq -e "${"$" + "{@:2}"}" < output || {
              cat output 1>&2
              echo ""    1>&2
              fail "$1"
            }
          }

          q "Got object" 'type | . == "object"'
          q "Got sizes"  'keys | sort | . == ["1", "2", "3"]'
          for SIZE in 1 2 3
          do
            q "Got object for '$SIZE'" \
              --arg size "$SIZE" '.[$size] | type | . == "object"'

            q "Got reps for '$SIZE'" \
              --arg size "$SIZE" '.[$size] | keys | sort | . == ["0", "1"]'

            for REP in 0 1
            do
              for FIELD in stdout stderr timeout error 'timed out'
              do
                q "Size '$SIZE' rep '$REP' has '$FIELD'" \
                  --arg size "$SIZE" --arg rep "$REP" --arg field "$FIELD" \
                  '.[$size] | .[$rep] | has($field)'
              done
            done
          done
          mkdir "$out"
        '';

      find-something = runCommand "cutoff-finds-something"
        {
          buildInputs = [ fail jq ];
          cmd = cutoff-timer {
            runners      = mkRunners [3] 3;
            timeout_secs = 600;
          };
        }
        ''
          set -e
          "$cmd" | tee "$out"

          jq -e '[.[] | .[] | .["timed out"]] | all | not' < "$out" ||
            fail "All timed out"

          function outs {
            jq '.[] | .[] | select(.["timed out"] | not) | .stdout' < "$out"
          }

          function eqs {
            outs | jq -R '.' | jq '.[]' | jq -s '. | length' 1>&2
          }

          eqs
          exit 1
        '';

      real = cutoff-timer {
        runners = mkRunners [1 10 20 30 40 50] 5;
        timeout_secs = 3600;
      };
    };
    withDeps [ times-out have-output find-something ] real;

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

    undefs  = find-undefined-cases      { te-benchmark = teb; };

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

    undefined = runCommand "undefined-${label}"
      {
        inherit namesFile undefs;
        buildInputs = [ jq ];
        expr        = writeScript "undef.jq" ''
          def mkArgs: [range(.)] | map(tostring | "free" + .) | join(" ");

          def mkCons: . as $in | keys | map(. + " " + ($in[.] | mkArgs));

          def render: "Trm.change_frees_to_fresh_vars @{term \"" + . + "\"}";

          def mkFuns: . as $in |
                      .cons | mkCons |
                      map($in["name"] + " (" + . + ")" | render);

          map(. as $name | select($undefs | has($name)) |
              {name: ., cons: $undefs[.]} | mkFuns | .[]) | join(",\n")
        '';
      }
      ''
        set -e
        set -o pipefail

        jq -R '.' < "$namesFile" |
          jq -s -r --argfile undefs "$undefs" -f "$expr" > "$out"
      '';
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

      if [[ -n "$DUMP_SAMPLE" ]]
      then
        cat "$sampleFile"
        exit 0
      fi

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
      checkConjectures = { count, name, names }: runCommand
        "${name}-runner-test"
        {
          n           = name;
          c           = toString count;
          buildInputs = [ fail jq ];
          script      = runnerFor {
            inherit names te-benchmark;
            label = "test-${name}";
          };
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

      namesMatching = { label, wanted, unwanted }: runCommand
        "names-for-${label}"
        {
          inherit wanted unwanted;
          buildInputs = [ jq te-benchmark.tools ];
          data        = make-tebenchmark-data { inherit te-benchmark; };
        }
        ''
          function encodedNames {
            jq -r '.types | keys | .[]' < "$data"
          }

          function withDecoded {
            paste <(encodedNames) <(encodedNames | decode)
          }

          for WPAT in $wanted
          do
            CHOSEN=$(withDecoded | grep "$WPAT")
            echo -e "Whittling down these names for '$WPAT':\n$CHOSEN" 1>&2
            for UPAT in $unwanted
            do
              CHOSEN=$(echo "$CHOSEN" | grep -v "$UPAT")
            done
            echo "$CHOSEN" | cut -f1 | tee "$out"
          done
        '';

      nat = {
        count = "";  # Find at least one conjecture
        name  = "nat";
        names = namesMatching {
          label    = "nat";
          wanted   = [ "plus$"       "mult$"  ];
          unwanted = [ "bin_distrib" "regexp" ];
        };
      };

      list = {
        count = 1;
        name  = "list";
        names = namesMatching {
          label    = "list";
          wanted   = [ "rev$"          ];
          unwanted = [ "qrev" "regexp" ];
        };
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

      tests = [
        functionDefsMatchUp
        (checkConjectures nat)
        (checkConjectures list)
      ];
    };
    withDeps tests nothing;
}
