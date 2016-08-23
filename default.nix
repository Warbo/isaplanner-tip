{ pkgs ? import <nixpkgs> {} }:

assert pkgs.haskellPackages ? callHackage ||
       abort "haskellPackages doesn't have callHackage; nixpkgs too old?";

with pkgs; rec {

  polyml = pkgs.polyml.overrideDerivation (old: {
    name = "polyml-5.5.2";
    src  = "${isabelle2015}/contrib/polyml-5.5.2-3/src";
  });

  isabelle2015 = stdenv.mkDerivation {
    name = "isabelle2015";
    src  = fetchurl {
      url    = "http://isabelle.in.tum.de/website-Isabelle2015/dist/Isabelle2015_linux.tar.gz";
      sha256 = "13kqm458d8mw7il1zg5bdb1nfbb869p331d75xzlm2v9xgjxx862";
    };
    installPhase = ''
      cp -a . "$out"
    '';
  };

  isaplib = stdenv.mkDerivation {
    name = "isaplib";
    src  = fetchgit {
      url    = https://github.com/iislucas/isaplib.git;
      rev    = "714e0a2";
      sha256 = "04jnw51718fzhw6b14fbff99x6d2k59zqicqs6lv78ysiys8z76y";
    };
    inherit isabelle2015;
    installPhase = ''
      PLIB="$PWD"
      cd ..
      cp -a "$isabelle2015" ./isabelle
      chmod +w -R ./isabelle
      cp -a "$PLIB" ./isabelle/contrib/isaplib
      cp -a ./isabelle "$out"
    '';
  };

  isaplanner = stdenv.mkDerivation {
    name = "isaplanner";
    src  = fetchFromGitHub {
      owner  = "TheoryMine";
      repo   = "IsaPlanner";
      rev    = "f3f4220";
      sha256 = "0kw1qslzv1a7fq25rhashcmnsnp16vfy92y3n0mccqs5ll2wf4f4";
    };

    inherit isaplib;
    buildInputs = [ file jdk perl polyml nettools ];

    postPatch = (isabelle.override { inherit polyml; }).postPatch;

    passAsFile = [ "postPatch" ];

    patchPhase = ''
      ORIG="$PWD"
      cd ..

      mv "$ORIG" IsaPlanner

      cp -r "$isaplib" "$ORIG"
      chmod +w -R "$ORIG"

      mv IsaPlanner "$ORIG/contrib"

      cd "$ORIG"

      echo "Patching with '$postPatchPath'" 1>&2
      . "$postPatchPath"
    '';

    buildPhase = ''
      set -e
      ISABELLE_DIR="$PWD"
      ISABELLE_JDK_HOME=$(dirname "$(dirname "$(command -v javac)")")

      export ISABELLE_JDK_HOME
      HOME=$(dirname "$PWD")
      export HOME

      cd contrib/IsaPlanner
      ../../bin/isabelle build -d . HOL-IsaPlannerSession
      ../../bin/isabelle build -d . IsaPlanner-Test
    '';
    installPhase = ''
      cp -a "$ORIG" "$out"
    '';
  };

  isabelle-tip = stdenv.mkDerivation {
    name = "isabelle-tip";

    buildInputs = let haskellPackages  = haskell.packages.ghc7103;
                      te-benchmark-src = fetchgit {
                        url    = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
                        rev    = "b668e34";
                        sha256 = "1vabsxdli29i8mxrn61n9yqb5psysc8xq1g7vz13lfymv2a0ypbd";
                      };
                      te-benchmark = callPackage "${te-benchmark-src}" {
                        inherit haskellPackages;
                      };
                   in [ te-benchmark.tip-benchmark-smtlib
                        (haskellPackages.ghcWithPackages (h: [ h.tip-lib ])) ];

    buildCommand = ''
      source $stdenv/setup

      F=$(completeTipSig)

      echo "Converting smtlib data in '$F' into isabelle code" 1>&2
      tip --isabelle < "$F" > "$out"
    '';
  };

  isaplanner-tip =
    let theory = writeScript "Invoke.thy" ''
                   theory Invoke
                   imports A
                   begin
                   end
                 '';
     in stdenv.mkDerivation {
          name = "isaplanner-tip";

          inherit isabelle-tip;

          buildInputs = [ isaplanner ];

          buildCommand = ''
            source $stdenv/setup

            # Theory name must match file name; 'tip' uses the name "A"
            cp "$isabelle-tip" "A.thy"
            cp "$theory" "Invoke.thy"

            echo 'use_thy "Invoke";' | isabelle console
          '';
        };

  bench = with haskellPackages;
          callPackage (callHackage "bench" "1.0.1") {};

  isacosy-nat =
    let theoryName = "IsaCoSyNat";
        theory = writeScript "${theoryName}.thy" ''
          theory ${theoryName}
          imports Main IsaP IsaCoSy Orderings Set Pure List
          begin

          ML {*
          val datatypes = [@{typ "nat"}, @{typ "nat list"}];
          val functions = map Term.dest_Const
            [@{term "Groups.plus_class.plus :: nat => nat => nat"},
            @{term "Groups.minus_class.minus :: nat => nat => nat"},
            @{term "Groups.times_class.times :: nat => nat => nat"},
            @{term "List.append :: nat list => nat list => nat list"}];
          val def_thrms = [@{thms "Nat.plus_nat.simps"}, @{thms "Nat.minus_nat.simps"},  @{thms
          "Nat.times_nat.simps"},
                       @{thms "List.append.simps"}];
          val thrms = flat def_thrms;
          val fundefs = functions ~~ def_thrms;
          (* Don't want to synthesise undefined terms *)
          val constr_trms = [Trm.change_frees_to_fresh_vars @{term "hd([])"},
                             Trm.change_frees_to_fresh_vars @{term "tl([])"}
            ];

          (* set constraint params *)
            val cparams0 =
                ConstraintParams.empty
                  |> ThyConstraintParams.add_eq @{context}
                  |> ThyConstraintParams.add_datatype' @{context} @{typ "nat"}
                  |> ThyConstraintParams.add_datatype' @{context} @{typ "nat list"}
                  |> (ConstraintParams.add_consts functions)
                  |> ConstraintParams.add_arb_terms @{context} constr_trms
                  (*|> ConstraintParams.add_thms @{context} thrms*);
          val (init_ctxt, cparams) =
            ConstraintParams.add_ac_properties_of_consts @{context} fundefs cparams0;
           ConstraintParams.print init_ctxt cparams;

          val thy_constraints = (Constraints.init init_ctxt cparams);
          val top_term = Thm.term_of @{cpat "op = :: ?'a => ?'a => bool"};
          val top_const = (Constant.mk (fst (Term.dest_Const top_term)));
          val dummy_prover = SynthInterface.Prover(fn ctxt => fn term => (NONE,[]));

          val (nw_cparams, nw_ctxt) = SynthInterface.thm_synth
            SynthInterface.rippling_prover
            SynthInterface.quickcheck
            SynthInterface.wave_rule_config
            SynthInterface.var_allowed_in_lhs
            {max_size = 8, min_size = 3, max_vars = 3, max_nesting= SOME 2}
            (Constant.mk "HOL.eq") (cparams0,@{context});
          (*map (Trm.print nw_ctxt) (SynthOutput.get_all (SynthOutput.Ctxt.get nw_ctxt));*)

          map (Trm.print nw_ctxt) (SynthOutput.get_conjs (SynthOutput.Ctxt.get nw_ctxt));
          *}

          end
        '';
       explore = writeScript "explore.sh" ''
         [[ -d "$ISAPLANNER_DIR" ]] || {
           echo "ISAPLANNER_DIR '$ISAPLANNER_DIR' not found" 1>&2
           exit 1
         }
         echo 'use_thy "${theoryName}";' |
           isabelle console -d "$ISAPLANNER_DIR" -l HOL-IsaPlannerSession
       '';
    in stdenv.mkDerivation {
         name = "isacosy-nat";
         buildInputs = [ perl isaplanner moreutils bench ];
         inherit isaplanner theory glibcLocales;

         buildCommand = ''
           source $stdenv/setup
           set -x

           # Theory name must match file name; 'tip' uses the name "A"
           cp "$theory" "${theoryName}.thy"

           # Force Haskell to use UTF-8, or else we get I/O errors
           export LANG="en_US.UTF-8"

           # Stop Perl complaining about unset locale variables
           export LOCALE_ARCHIVE="$glibcLocales/lib/locale/locale-archive"

           # Used as a temporary directory by Isabelle
           export HOME="$PWD"

           # Build IsaPlanner on its own, to avoid benchmarking it
           export ISAPLANNER_DIR="$isaplanner/contrib/IsaPlanner"
           isabelle build -d "$ISAPLANNER_DIR" -l HOL-IsaPlannerSession

           # Benchmark IsaCoSy, using the build of IsaPlanner from above
           bench "${explore}" --raw times

           # Get equations

           function stripPrefix {
             # Get everything after the last "Adding ..." message. We use tac to
             # put the lines in reverse order, grep to get (up to a million of)
             # the lines 'before' the 'first' occurrence of "Adding ...", put
             # back into order with tac, then trim off the "Adding ..." line with
             # tail
             tac | grep -m 1 -B 1000000 | tac | tail -n +2
           }

           function stripSuffix {
             # Get everything before the first "val ..." line, which indicates
             # the start of an ML code dump. Also strip out any "###" warnings
             grep -m 1 -B 1000000 "^val " | head -n -1 | grep -v "^### "
           }

           ${explore} | stripSuffix | stripPrefix > equations

           # Store results
           mkdir "$out"
           cp equations "$out"/
           cp times     "$out"/
         '';
  };
}
