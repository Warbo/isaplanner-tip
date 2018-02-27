{ cutoff-timer, fail, jq, lib, lzip, nothing, runCommand, runnerForSample,
  sampleAnalyser, withDeps, writeScript }:

with builtins;
with lib;
rec {
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
              for FIELD in stdout stderr time timeout error 'timed out'
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
            jq -r '.[] | .[] | select(.["timed out"] | not) | .stdout' < "$out"
          }

          function eqs {
            outs | jq '.[]' | jq -s '. | length'
          }

          COUNT=$(eqs)
          [[ "$COUNT" -gt 0 ]] || fail "Found no equations"
        '';

      real = cutoff-timer {
        runners      = mkRunners [1 2 4 8 16 32] 5;
        timeout_secs = 3600;
      };
    };
    withDeps [ times-out have-output find-something ] real;

  # We might as well check that the equations found when measuring cutoff times
  # are actually valid
  analyse-results = rec {
    outputs = runCommand "cutoff-output"
      {
        buildInputs       = [ fail jq lzip ];
        compressedResults = ./results/cutoff-times-46d5022.json.lz;
        nixExpr           = writeScript "cutoff-output.nix" ''
          with builtins;
          {
            meta = fromJSON (readFile ./meta.json);
            paths = import
        '';
      }
      ''
        set -e

        function data {
          lzip -d < "$compressedResults"
        }

        function sizes {
          data | jq -r 'keys | join(" ")'
        }

        function reps {
          data | jq -r --arg size "$SIZE" '.[$size] | keys | join(" ")'
        }

        function query {
          data | jq "$@" --arg s "$SIZE" --arg r "$REP" ".[\$s] | .[\$r] | $Q"
        }

        function stdout {
          Q='.stdout' query -r
        }

        echo "Checking that we got JSON" 1>&2
        for SIZE in $(sizes)
        do
          for REP in $(reps)
          do
            stdout | jq -e 'type | . == "array"' > /dev/null ||
              fail "Error reading stdout of size $SIZE rep $REP"
          done
        done

        # Generate a default.nix file storing details of each run. Each stdout
        # is stored as a separate file 'stdout-foo-bar.json', and referenced as
        # a relative path in default.nix

        mkdir "$out"

        function genNix {
          echo '{'
            for SIZE in $(sizes)
            do
              echo "\"$SIZE\" = {"
                for REP in $(reps)
                do
                  echo "Processing size $SIZE, rep $REP..." 1>&2
                  stdout "$SIZE" "$REP" > "$out/stdout-$SIZE-$REP.json"
                  echo "\"$REP\" = {"
                    echo "size   = $SIZE;"
                    echo "rep    = $REP;"
                    echo "stdout = ./stdout-$SIZE-$REP.json;"

                    ERR=$(Q='.error' query)
                    echo "error = $ERR;"

                    SAMPLE=$(Q='.sample' query)
                    echo "sample = $SAMPLE;"
                  echo '};'
                done
              echo '};'
            done
          echo '}'
        }

        genNix > "$out/default.nix"
      '';

    meta = import outputs;

    analyse = data:
      with data;
      with rec {
        go = given: runCommand "cutoff-analyser-${toString size}-${toString rep}"
          {
            stdout = given;
            script = sampleAnalyser {
              REP           = toString rep;
              SIZE          = toString size;
              SAMPLED_NAMES = sample;
            };
          }
          ''
            "$script" < "$stdout" > "$out"
          '';

        empty = writeScript "empty.json" "[]";

        result = go (if error then empty else stdout);
      };
      data // { analysis = result; };

    analysis = mapAttrs (_: mapAttrs (_: analyse)) meta;

    checks =
      with rec {
        haveJson = result: runCommand
          "have-json-${toString result.size}-${toString result.rep}"
          {
            inherit (result) stdout analysis;
            buildInputs = [ fail jq ];
          }
          ''
            set -e
            jq -e 'type | . == "array"' < "$stdout" ||
              fail "Stdout not JSON array:\n$(cat "$stdout")"

            jq -e 'type | . == "object"' < "$analysis" ||
              fail "Analysis not JSON object:\n$(cat "$analysis")"

            mkdir "$out"
          '';

        runsGetJson = map haveJson actualRuns;

        actualRuns = concatMap attrValues (attrValues analysis);

        havePrecisionRecall = runCommand "have-prec-rec"
          {
            analyses = concatStringsSep "\n" (map (x: x.analysis) actualRuns);
            buildInputs = [ fail jq ];
          }
          ''
            GOT_PREC=0
            GOT_REC=0
            while read -r AN
            do
              jq -e 'has("precision")'         < "$AN" > /dev/null
              jq -e 'has("recall")'            < "$AN" > /dev/null
              jq -e '.wanted | length | . > 0' < "$AN" > /dev/null

              if [[ "$GOT_PREC" -eq 0 ]]
              then
                if jq -e '.precision | . == []' < "$AN" > /dev/null
                then
                  true
                else
                  if jq -e '.precision | . > 0' < "$AN" > /dev/null
                  then
                    GOT_PREC=1
                  fi
                fi
              fi

              if [[ "$GOT_REC" -eq 0 ]]
              then
                if jq -e '.recall | . > 0' < "$AN" > /dev/null
                then
                  GOT_REC=1
                fi
              fi
            done < <(echo "$analyses")

            [[ "$GOT_PREC" -eq 1 ]] || fail "No non-zero precisions"
            [[ "$GOT_REC"  -eq 1 ]] || fail "No non-zero recalls"
            mkdir "$out"
          '';
      };
      assert length (filter (x: !x.error) actualRuns) > 0 ||
             abort "No cutoff runs succeeded?";
      runsGetJson ++ [ havePrecisionRecall ];
  };
}
