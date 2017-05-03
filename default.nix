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
    buildInputs = [ file jdk makeWrapper nettools perl polyml ];

    isaplanner_bin = writeScript "isaplanner" ''
      #!${bash}/bin/bash
      # -n tells Isabelle not to build the base theories (Pure, HOL, IsaPlanner,
      #    etc.). We do this for two reasons: we've already built these theories
      #    during installation, so it's a big waste of time; it also lets us use
      #    read-only directories, like Nix store paths, for our HOME.
      # -d tells Isabelle where to find its the base theories.
      # -l tells Isabelle which base theory we want to use.
      isabelle console -n -d "$ISAPLANNER_DIR" -l HOL-IsaPlannerSession "$@"
    '';

    ISABELLE_JDK_HOME = jdk;

    postPatch  = (isabelle.override { inherit polyml; }).postPatch;
    passAsFile = [ "postPatch" ];
    patchPhase = ''
      echo "Moving source into standalone directory" 1>&2
      mkdir -p ./given-src

      for F in ./*
      do
        NAME=$(basename "$F")
        [[ "x$NAME" = "xgiven-src" ]] || mv "$F" ./given-src
      done

      echo "Making mutable copy of isaplib" 1>&2
      cp -r "$isaplib" ./isaplib
      chmod +w -R ./isaplib

      echo "Moving source to contrib/IsaPlanner" 1>&2
      mv ./given-src ./isaplib/contrib/IsaPlanner

      echo "Patching with '$postPatchPath'" 1>&2
      pushd ./isaplib
      . "$postPatchPath"
      popd
    '';

    buildPhase = ''
      set -e

      echo "Setting build environment" 1>&2

      ISABELLE_DIR="$PWD/isaplib"
      export ISABELLE_DIR

      mkdir ./mutable-home
      HOME="$PWD/mutable-home"
      export HOME

      echo "Building" 1>&2

      pushd "$ISABELLE_DIR/contrib/IsaPlanner"
      ../../bin/isabelle build -d . HOL-IsaPlannerSession
      ../../bin/isabelle build -d . IsaPlanner-Test
      popd
    '';

    installPhase = ''
      echo "Setting install environment" 1>&2

      mkdir -p "$out/bin"

      cp -a "$ISABELLE_DIR" "$out/isabelle_dir"
      export ISABELLE_DIR="$out/isabelle_dir"

      cp -a "$HOME" "$out/home"
      export HOME="$out/home"

      echo "Installing isaplanner binary" 1>&2
      makeWrapper "$isaplanner_bin" "$out/isabelle_dir/bin/isaplanner" \
        --set ISAPLANNER_DIR "$out/isabelle_dir/contrib/IsaPlanner"

      echo "Wrapping binaries in required environment" 1>&2
      for F in "$out"/isabelle_dir/bin/*
      do
        # ISABELLE_JDK_HOME is the Java VM to use (for Scala, jEdit, etc.)
        # HOME is for caches, settings, etc. Hopefully these are pre-populated.
        # ISABELLE_DIR is for Isabelle's binaries, etc.
        makeWrapper "$F" "$out/bin/$(basename "$F")"   \
          --set ISABELLE_JDK_HOME "$ISABELLE_JDK_HOME" \
          --set HOME              "$HOME"              \
          --set ISABELLE_DIR      "$ISABELLE_DIR"
      done
    '';
  };

  isabelle-tip =
    with rec {
      haskellPackages  = haskell.packages.ghc7103;
      te-benchmark-src = fetchgit {
        url    = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
        rev    = "9642c88";
        sha256 = "0874bhvsifyrg3gb30k7ic2w3ms6ak26apf1rv0x2bmrqmzav6gj";
      };
      te-benchmark = callPackage "${te-benchmark-src}" {
        inherit haskellPackages;
      };
    };
    stdenv.mkDerivation {
      name         = "isabelle-tip";
      buildInputs  = [ (haskellPackages.ghcWithPackages (h: [ h.tip-lib ]))
                       isaplanner jq perl  vim ];
      smtdata      = te-benchmark.tip-benchmark-smtlib;
      FIXES        = ./fixes.json;
      preprocess   = ./preprocess.sh;
      postprocess  = ./postprocess.sh;
      buildCommand = ''
        source $stdenv/setup
        set -e
        set -o pipefail

        echo "Converting smtlib data in '$smtdata' into isabelle code" 1>&2
        "$preprocess" < "$smtdata" | tip --isabelle | "$postprocess" > A.thy

        echo "Testing"
        OUTPUT=$(echo 'use_thy "A";' | isaplanner)

        if echo "$OUTPUT" |  grep -i -C 10 'error' > /dev/null
        then
           echo "Fail: Errors found in generated theory" 1>&2
           echo "$OUTPUT" 1>&2
           cp -v "A.thy" /tmp/A.thy
           exit 1
        fi
        echo "Passed"

        mkdir -p "$out"
        mv A.thy "$out/"
      '';
    };

  haskellTypesOf = haskell-tip: runCommand "haskell-types-of"
    {
      buildInputs = [ makeWrapper ];
      raw = writeScript "haskell-types-of-raw" ''
        #!/usr/bin/env bash
        while read -r NAME
        do
          jq --arg name "$NAME" \
          'map(select(.name == $name)) | .[] | .type' < "${haskell-tip}"
        done | jq -s '.'
      '';
    }
    ''
      makeWrapper "$raw" "$out" --prefix PATH : "${jq}/bin"
    '';

  isacosy-template = ''
    theory ISACOSY

    imports Main IsaP IsaCoSy Orderings Set Pure List A
    begin

    ML {*
    val datatypes = [@{typ "nat"}, @{typ "nat list"}];
    val functions = map Term.dest_Const [
      @{term "Groups.plus_class.plus :: nat => nat => nat"},
      @{term "Groups.minus_class.minus :: nat => nat => nat"},
      @{term "Groups.times_class.times :: nat => nat => nat"},
      @{term "List.append :: nat list => nat list => nat list"}
    ];

    val def_thrms = [
      @{thms  "Nat.plus_nat.simps"},
      @{thms "Nat.minus_nat.simps"},
      @{thms "Nat.times_nat.simps"},
      @{thms   "List.append.simps"}
    ];

    val fundefs = functions ~~ def_thrms;

    (* Don't want to synthesise undefined terms *)
    val constr_trms = [
      Trm.change_frees_to_fresh_vars @{term "hd([])"},
      Trm.change_frees_to_fresh_vars @{term "tl([])"}
    ];

    (* Set constraints *)
    val cparams = ConstraintParams.empty
                |> ThyConstraintParams.add_eq @{context}
                |> ThyConstraintParams.add_datatype' @{context} @{typ "nat"}
                |> ThyConstraintParams.add_datatype' @{context} @{typ "nat list"}
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
         echo 'use_thy "${theoryName}";' | isaplanner
       '';
    in stdenv.mkDerivation {
         name = "isacosy-nat";

         inherit isaplanner theory;

         # Force Haskell to use UTF-8, or else we get I/O errors
         LANG = "en_US.UTF-8";

         # Stop Perl complaining about unset locale variables
         LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";

         buildInputs  = [ perl isaplanner moreutils bench gnugrep ];
         buildCommand = ''
           source $stdenv/setup
           set -x

           # Theory name must match file name; 'tip' uses the name "A"
           cp "$theory" "${theoryName}.thy"

           mkdir -p "$out"

           # Benchmark IsaCoSy
           bench "${explore}" --raw "$out/times"

           "${explore}" > "$out/output"
         '';
  };

  isacosy-nat-eqs = stdenv.mkDerivation {
    name = "isacosy-nat-eqs";
    data = isacosy-nat;

    buildCommand = ''
      source $stdenv/setup

      set -e

      [[ -f "$data/output" ]] || {
        echo "Error: Isabelle output '$data/output' doesn't exist" 1>&2
        exit 1
      }

      function stripPrefix {
        # Get everything after the last "Adding ..." message. We use tac to
        # put the lines in reverse order, grep to get (up to a million of)
        # the lines 'before' the 'first' occurrence of "Adding ...", put
        # back into order with tac, then trim off the "Adding ..." line with
        # tail
        echo "Stripping prefix" 1>&2
        tac | grep -m 1 -B 1000000 "^Adding " | tac | tail -n +2
        echo "Stripped prefix" 1>&2
      }

      function stripSuffix {
        # Get everything before the first "val ..." line, which indicates
        # the start of an ML code dump. Also strip out any "###" warnings
        echo "Stripping suffix" 1>&2
        grep -m 1 -B 1000000 "^val " | head -n -1 | grep -v "^### "
        echo "Stripped suffix" 1>&2
      }

      mkdir -p "$out"

      stripSuffix < "$data/output" > noSuff
      stripPrefix < noSuff         > "$out/equations"

      cp "$data/times" "$out/times"
    '';
  };
}
