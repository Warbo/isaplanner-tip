{ allDrvsIn, attrsToDirs, bash, eqsToJson, extractEqs, isaplanner, lib, mkBin,
  replace, runCommand, te-benchmark, withDeps, wrap, writeScript }:

with lib;
rec {
  findNatEqs = runCommand "isacosy-nat-eqs"
    {
      raw = isacosy-untested;
      dir = attrsToDirs {
        "ISACOSY.thy" = isacosy-theory-strings {
          name        = "nat-example";
          datatypes   = ''|> ThyConstraintParams.add_datatype' @{context} @{typ "nat"}'';
          definitions = ''
            @{thms "Nat.plus_nat.simps"},
            @{thms "Nat.minus_nat.simps"},
            @{thms "Nat.times_nat.simps"}
          '';
          functions = ''
            @{term "Groups.plus_class.plus   :: nat => nat => nat"},
            @{term "Groups.minus_class.minus :: nat => nat => nat"},
            @{term "Groups.times_class.times :: nat => nat => nat"}
          '';
          imports   = "";
          undefined = "";
        };
      };
    }
    ''
      set -e
      set -o pipefail

      # Theory name must match file name; 'tip' uses the name "A"
      cd "$dir" || {
        echo "Couldn't cd to '$dir'" 1>&2
        exit 1
      }

      OUTPUT=$("$raw" "ISACOSY.thy")

      EQS=$(echo "$OUTPUT" | grep -c '=')
      [[ "$EQS" -gt 2 ]] || {
        echo "$OUTPUT" 1>&2
        echo "Didn't find equations for +, - and *:" 1>&2
        exit 1
      }
      mkdir "$out"
    '';

  isacosy-untested = wrap {
    name   = "isacosy-raw";
    paths  = [ isaplanner ];
    vars   = { inherit extractEqs; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      [[ -n "$1" ]] || {
        echo "No file given, aborting" 1>&2
        exit 1
      }

      function maybeShowRaw {
        if [[ -n "$SHOW_RAW" ]]
        then
          echo "Sending raw equations to stderr" 1>&2
          tee >(cat 1>&2)
        else
          cat
        fi
      }

      THY=$(basename "$1" .thy)
      echo "use_thy \"$THY\";" | isaplanner | maybeShowRaw | "$extractEqs"
    '';
  };

  isacosy = withDeps (allDrvsIn { inherit findNatEqs; })
                     (mkBin {
                       name  = "isacosy";
                       paths = [ bash ];
                       vars  = {
                         inherit eqsToJson;
                         raw = isacosy-untested;
                       };
                       script = ''
                         #!/usr/bin/env bash
                         set -e
                         set -o pipefail

                         "$raw" "$@" | "$eqsToJson"
                       '';
                     });

  isacosy-theory-strings =
    { datatypes, definitions, functions, imports ? "A", name, undefined }:
      isacosy-theory {
        inherit name;
        datatypes   = writeScript "datatypes"   datatypes;
        definitions = writeScript "definitions" definitions;
        functions   = writeScript "functions"   functions;
        imports     = writeScript "imports"     imports;
        undefined   = writeScript "undefined"   undefined;
      };

  isacosy-theory = {
    datatypes,
    definitions,
    functions,
    imports ? (writeScript "imports" "A"),
    name,
    undefined
  }: runCommand "isacosy-theory-${name}"
       {
         inherit datatypes definitions functions imports undefined;
         template    = ./isacosy-template.thy;
         buildInputs = [ replace ];
       }
       ''
         set -e

         function doReplace {
           [[ -f ./temp ]] || {
             echo "No ./temp file found, aborting" 1>&2
             exit 1
           }

           VAL=$(cat "$2")
           replace "(*TEMPLATE_REPLACE_ME_WITH_$1*)" "$VAL" < ./temp > ./temp2

           rm ./temp
           mv ./temp2 ./temp
         }

         cp "$template" ./temp

         doReplace "DATATYPES"   "$datatypes"
         doReplace "DEFINITIONS" "$definitions"
         doReplace "FUNCTIONS"   "$functions"
         doReplace "IMPORTS"     "$imports"
         doReplace "UNDEFINED"   "$undefined"

         mv ./temp "$out"
       '';
}
