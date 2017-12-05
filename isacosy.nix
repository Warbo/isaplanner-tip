{ isaplanner, makeWrapper, runCommand, writeScript }:

rec {
  isacosy = runCommand "isacosy"
    {
      buildInputs = [ makeWrapper ];
      raw = writeScript "isacosy-raw" ''
        #!/usr/bin/env bash
        set -e
        set -o pipefail

        [[ -n "$1" ]] || {
          echo "No file given, aborting" 1>&2
          exit 1
        }
        THY=$(basename "$1" .thy)
        echo "use_thy \"$THY\";" | isaplanner | "${./extract_eqs.sh}"
      '';
    }
  ''
    mkdir -p "$out/bin"
    makeWrapper "$raw" "$out/bin/isacosy" --prefix PATH : "${isaplanner}/bin"
  '';

  isacosy-template = writeScript "isacosy-template" ''
    theory ISACOSY

    imports Main IsaP IsaCoSy Orderings Set Pure List A
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

      (* Undefined terms, eg. Trm.change_frees_to_fresh_vars @{term "hd([])"} *)
      val constr_trms = [
        (*TEMPLATE_REPLACE_ME_WITH_UNDEFINED*)
      ];

      (* Add variables for each datatype, e.g.
           ThyConstraintParams.add_datatype' @{context} @{typ "nat"} *)
      val cparams = ConstraintParams.empty
                  |> ThyConstraintParams.add_eq @{context}
                  (*TEMPLATE_REPLACE_ME_WITH_DATATYPES*)
                  |> ConstraintParams.add_consts functions
                  |> ConstraintParams.add_arb_terms @{context} constr_trms

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

  isacosy-theory = { datatypes, definitions, functions, name, undefined }:
    runCommand "isacosy-theory-${name}"
      {
        inherit datatypes definitions functions undefined;
        template = isacosy-template;
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
        replace "FUNCTIONS"   "$functions"
        replace "DEFINITIONS" "$definitions"
        replace "UNDEFINED"   "$undefined"
        replace "DATATYPES"   "$datatypes"
        mv ./temp "$out"
      '';
}
