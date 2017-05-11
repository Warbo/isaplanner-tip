with rec {
  defs = pkgs: import ./. { inherit pkgs; };

  helpers = pkgs: with pkgs; with defs pkgs; rec {
    haskell-te-src = latestGit {
      url = "http://chriswarbo.net/git/haskell-te.git";
    };

    haskell-te = (import "${haskell-te-src}").allDefs;

    tip-benchmarks = latestGit {
      url = http://chriswarbo.net/git/theory-exploration-benchmarks.git;
    };
  };

  results = pkgs: tests pkgs // { debug = defs // helpers; };

  tests = pkgs: with pkgs; with defs pkgs; with helpers pkgs; rec {
    haskellTypesOfTip = runCommand "haskell-types-of-tip"
      {
        typesof     = haskellTypesOf haskell-te.tipBenchmarks.annotatedAsts;
        annotated   = haskell-te.tipBenchmarks.annotatedAsts;
        buildInputs = [ haskell-te.tipBenchmarks.tools jq ];
      }
      ''
        set -e
        echo "Checking we can get Haskell types of sampled names" 1>&2

        NAMES=$(choose_sample 219 0 | shuf | head -n10)
        echo "NAMES: $NAMES" 1>&2

        TYPES=$(echo "$NAMES" | "$typesof")
        echo "TYPES: $TYPES" 1>&2

        while read -r NAME
        do
          TYPE=$(jq -r --arg name "$NAME" \
                    'map(select(.name == $name)) | .[] | .type' < "$annotated")
          echo "$TYPES" |
            jq -e --arg type "$TYPE" 'any(. == $type)' > /dev/null || {
            echo "Didn't find type of '$NAME' ($TYPE) in output:" 1>&2
            echo "$TYPES" 1>&2
            exit 1
          }
        done < <(echo "$NAMES")

        echo "Pass" > "$out"
      '';

    translateTypes = runCommand "translate-types"
      {
        inherit haskellToIsabelleTypes;
        buildInputs = [ haskell-te.tipBenchmarks.tools ];
        typesof     = haskellTypesOf haskell-te.tipBenchmarks.annotatedAsts;
      }
      ''
        echo "Checking we can translate Haskell types to Isabelle types" 1>&2

         NAMES=$(choose_sample 10 0)
        HTYPES=$(echo "$NAMES"  | "$typesof")
        ITYPES=$(echo "$HTYPES" | "$haskellToIsabelleTypes")

        echo -e "Isabelle types:\n$ITYPES" 1>&2

        if echo "$ITYPES" | grep "Integer"
        then
          echo "Translation should have replaced Integer" 1>&2
          exit 1
        fi

        if echo "$ITYPES" | grep -- '->'
        then
          echo "Translation should have replaced '->' with '=>'" 1>&2
          exit 1
        fi

        echo "Pass" > "$out"
      '';

    natSampleFindsEqs = runCommand "nat-sample-finds-eqs"
      {
        buildInputs = [
          isacosy
          haskell-te.tipBenchmarks.tools
          (make_isacosy_theory haskell-te.tipBenchmarks)
        ];
      }
      ''
        function getNames {
          echo "Gathering TIP names" 1>&2
          NAMES=""
          TIPNAMES=$(choose_sample 219 0)
          TIPTRANS=$(echo "$TIPNAMES" | decode)

          for N in plus times
          do
            while read -r PAIR
            do
              NEWNAME=$(echo "$PAIR" | cut -f1)
              NAMES=$(echo -e "$NAMES\n$NEWNAME" | grep '^.')
            done < <(paste <(echo "$TIPNAMES") <(echo "$TIPTRANS") | grep "$N")
          done

          echo -e "Found names:\n$NAMES" 1>&2
          echo "$NAMES" | sort -u
        }

        THY=$(getNames | make_isacosy_theory)
        RESULT=$(isacosy "$THY")
        EQS=$(echo "$RESULT" | grep -c '=')
        [[ "$EQS" -gt 2 ]] || {
          echo "Didn't find equations for + and *:" 1>&2
          echo "$RESULT" 1>&2
          exit 1
        }
      '';
  };
};
(import <nixpkgs> {}).lib.mapAttrs (_: results) (import ./pkgs.nix)
