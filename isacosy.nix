{ allDrvsIn, attrsToDirs, eqsToJson, extractEqs, isaplanner, lib, mkBin,
  runCommand, te-benchmark, withDeps, writeScript }:

with lib;
rec {
  isacosy-nat-eqs = runCommand "isacosy-nat-eqs"
    {
      buildInputs = [ isacosy-untested ];
      dir         = attrsToDirs {
        "ISACOSY.thy" = isacosy-theory-strings {
          name        = "nat-example";
          datatypes   = ''|> ThyConstraintParams.add_datatype' @{context} @{typ "nat"}'';
          definitions = ''
            @{thms "Nat.plus_nat.simps"},
            @{thms "Nat.minus_nat.simps"},
            @{thms "Nat.times_nat.simps"}
          '';
          functions = ''
            @{term "Groups.plus_class.plus :: nat => nat => nat"},
            @{term "Groups.minus_class.minus :: nat => nat => nat"},
            @{term "Groups.times_class.times :: nat => nat => nat"}
          '';
          imports   = "";
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

      isacosy "ISACOSY.thy" | tee "$out"
    '';

  isacosy-untested = mkBin {
    name   = "isacosy";
    paths  = [ isaplanner ];
    vars   = { inherit eqsToJson extractEqs; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      [[ -n "$1" ]] || {
        echo "No file given, aborting" 1>&2
        exit 1
      }
      THY=$(basename "$1" .thy)
      echo "use_thy \"$THY\";" | isaplanner | "$extractEqs" | "$eqsToJson"
    '';
  };

  isacosy = withDeps
    (allDrvsIn {
      natSampleFindsEqs = runCommand "nat-sample-finds-eqs"
        {
          result      = isacosy-nat-eqs;
          buildInputs = [
            isacosy-untested
            te-benchmark.tools
          ];
        }
        ''
          set -e
          EQS=$(grep -c '=' < "$result")
          [[ "$EQS" -gt 2 ]] || {
            echo "Didn't find equations for +, - and *:" 1>&2
            cat "$result" 1>&2
            exit 1
          }
          mkdir "$out"
        '';
    })
    isacosy-untested;

  isacosy-template = writeScript "isacosy-template" ''
    theory ISACOSY

    imports Main IsaP IsaCoSy Orderings Set Pure List (*TEMPLATE_REPLACE_ME_WITH_IMPORTS*)
    begin

    ML {*
      (* Example: @{term "Groups.plus_class.plus :: nat => nat => nat"} *)
      val functions = map Term.dest_Const [
        (*TEMPLATE_REPLACE_ME_WITH_FUNCTIONS*)
      ];

      (* Example: @{thms "Nat.plus_nat.simps"} *)
      val def_thrms = [
        (*TEMPLATE_REPLACE_ME_WITH_DEFINITIONS*)
      ];

      val fundefs = functions ~~ def_thrms;

      (* Add variables for each datatype, e.g.
           ThyConstraintParams.add_datatype' @{context} @{typ "nat"} *)
      val cparams = ConstraintParams.empty
                  |> ThyConstraintParams.add_eq @{context}
                  (*TEMPLATE_REPLACE_ME_WITH_DATATYPES*)
                  |> ConstraintParams.add_consts functions

      (* Perform the exploration *)
      val (_, nw_ctxt) = SynthInterface.thm_synth
        SynthInterface.rippling_prover
        SynthInterface.quickcheck
        SynthInterface.wave_rule_config
        SynthInterface.var_allowed_in_lhs
        {max_size = 8, min_size = 3, max_vars = 3, max_nesting = SOME 2}
        (Constant.mk "HOL.eq") (cparams, @{context});

      (* Extract the results *)
      val show           = Trm.pretty nw_ctxt;
      val result_context = SynthOutput.Ctxt.get nw_ctxt;

      val found_conjectures = map show
                                  (SynthOutput.get_conjs result_context);

      val found_theorems    = map (fn (_, thm) =>
                                     show (Thm.full_prop_of thm))
                                  (SynthOutput.get_thms result_context);

      (* Write output, delimited so we can easily chop off Isabelle/CoSy noise *)
      PolyML.print (Pretty.output NONE (Pretty.list "[" "]"
        ([Pretty.str "BEGIN OUTPUT"] @
         found_theorems              @
         found_conjectures           @
         [Pretty.str "END OUTPUT"])));
    *}
    end
  '';

  isacosy-theory-strings =
    { datatypes, definitions, functions, imports ? "A", name }:
      isacosy-theory {
        inherit name;
        datatypes   = writeScript "datatypes"   datatypes;
        definitions = writeScript "definitions" definitions;
        functions   = writeScript "functions"   functions;
        imports     = writeScript "imports"     imports;
      };

  isacosy-theory = {
    datatypes,
    definitions,
    functions,
    imports ? (writeScript "imports" "A"),
    name,
  }: runCommand "isacosy-theory-${name}"
       {
         template = isacosy-template;
         inherit datatypes definitions functions imports;
       }
       ''
         set -e

         function pre {
           grep -B 9999999 "TEMPLATE_REPLACE_ME_WITH_$1" < ./temp | head -n-1
         }

         function post {
           grep -A 9999999 "TEMPLATE_REPLACE_ME_WITH_$1" < ./temp | tail -n+2
         }

         function replace {
           [[ -f ./temp ]] || {
             echo "No ./temp file found, aborting" 1>&2
             exit 1
           }
           echo "Setting '$1' to '$2'" 1>&2
           pre  "$1" >  ./temp2
           cat  "$2" >> ./temp2
           post "$1" >> ./temp2
           rm ./temp
           mv ./temp2 ./temp
         }

         cp "$template" ./temp

         doReplace "DATATYPES"   "$datatypes"
         doReplace "DEFINITIONS" "$definitions"
         doReplace "FUNCTIONS"   "$functions"
         doReplace "IMPORTS"     "$imports"

         mv ./temp "$out"
       '';
}
