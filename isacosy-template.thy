theory ISACOSY

imports Main IsaP IsaCoSy Orderings Set Pure List (*TEMPLATE_REPLACE_ME_WITH_IMPORTS*)
begin

ML_command {*
  (* Example: @{term "Groups.plus_class.plus :: nat => nat => nat"} *)
  val functions = map Term.dest_Const [
    (*TEMPLATE_REPLACE_ME_WITH_FUNCTIONS*)
  ];

  (* The 'theorems' (equations) which define each of our terms. For things which
     use the 'function' keyword, try adding a '.simps' suffix. For things which
     use the 'definition' keyword, try adding a '_def' suffix.

     Examples: @{thms "Nat.zero_def"}
               @{thms "Nat.plus_nat.simps"} *)
  val def_thrms = [
    (*TEMPLATE_REPLACE_ME_WITH_DEFINITIONS*)
  ];

  (* Pair up functions with their definitions. These need to be kept in sync. *)
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

  (* Perform the exploration. NOTE: This will throw an Interrupt exception if
     we run out of memory. *)
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
  writeln (Pretty.output NONE (Pretty.list "[" "]"
    ([Pretty.str "BEGIN OUTPUT"] @
     found_theorems              @
     found_conjectures           @
     [Pretty.str "END OUTPUT"])));
*}

end
