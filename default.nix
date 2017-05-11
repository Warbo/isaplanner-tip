{ pkgs ? (import ./pkgs.nix {}).stable }:

with builtins;
with { pkgsAlias = pkgs; };  # Since 'with pkgs' shadows the name 'pkgs'
with pkgs;

assert haskellPackages ? callHackage ||
       abort "haskellPackages doesn't have callHackage; nixpkgs too old?";

rec {

  polyml = pkgsAlias.polyml.overrideDerivation (old: {
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
          --set ISABELLE_DIR      "$ISABELLE_DIR"      \
          --prefix PATH : "${perl}/bin"
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

  hackage = h: n: h.callPackage (runCabal2nix { url = "cabal://${n}"; });

  haskellToIsabelleTypes = runCommand "haskellToIsabelleTypes"
    rec {
      buildInputs = [ makeWrapper ];
      typeconvert = writeScript "typeConvert.hs" ''
        import Data.List
        import Language.Haskell.Exts.Parser
        import Language.Haskell.Exts.Pretty
        import Language.Haskell.Exts.Syntax

        main = interact convert

        -- Turns Haskell type signatures into Isabelle ones
        convert :: String -> String
        convert = render . parseSig

        -- Parse a type signature from a String
        parseSig :: String -> TypeSig
        parseSig s = case parseType s of
            ParseOk     t     -> typeToSig parseErr t
            ParseFailed _ err -> error err
          where parseErr msg = error (msg ++ ": " ++ s)

        -- Turns a subset of Haskell type signatures into our TypeSig format
        typeToSig :: (String -> TypeSig) -> Type b -> TypeSig
        typeToSig e t = case t of
          TyFun   _ i o -> collapseArrows i o
          TyApp   _ f x -> collapseApps   f x
          TyCon   _ c   -> Name (showName c)
          TyParen _ x   -> TSParen (typeToSig e x)

          TyVar{}    -> e "Type should have been monomorphised"
          TyForall{} -> e "Quantification not supported"
          TyTuple{}  -> e "Tuples not supported"
          TyList{}   -> e "Native lists not supported"
          TyInfix{}  -> e "Infix types not supported"
          _          -> e "Unsupported type wizardry"

        -- Unfolds 'a -> (b -> (c -> d))' into 'Arrow [a, b, c, d]'
        collapseArrows :: Type a -> Type a -> TypeSig
        collapseArrows i o = Arrow (typeToSig error i : process o)
          where process :: Type a -> [TypeSig]
                process (TyFun _ i2 o2) = typeToSig error i2 : process o2
                process x               = [typeToSig error x]

        collapseApps :: Type a -> Type a -> TypeSig
        collapseApps f x = TSApp (process f ++ [typeToSig error x])
          where process :: Type a -> [TypeSig]
                process (TyApp _ f2 x2) = process f2 ++ [typeToSig error x2]
                process t               = [typeToSig error t]

        -- Turns identifiers into Strings, discarding context, etc.
        showName :: QName a -> String
        showName n = case n of
          Qual   _ m x -> prettyPrint x
          UnQual _ x   -> prettyPrint x
          Special{}    -> error "'Special' names not supported"

        -- Takes a TypeSig and outputs an Isabelle representation
        render :: TypeSig -> String
        render s = case s of
          Name  n    -> n
          TSApp   sigs -> intercalate " " (rotate (map render sigs))
          TSParen sig  -> "(" ++ render sig ++ ")"
          Arrow sigs -> intercalate " => " (map render sigs)

        -- Moves the head of a list to the end (if there is one)
        rotate :: [a] -> [a]
        rotate []     = []
        rotate (x:xs) = xs ++ [x]

        data TypeSig = Name  String
                     | TSApp   [TypeSig]
                     | TSParen TypeSig
                     | Arrow [TypeSig]
      '';
      raw = writeScript "haskellToIsabelleTypes-raw" ''
        #!/usr/bin/env bash
        set -e
        set -o pipefail

        # Incoming type signatures may straddle multiple lines, so we require
        # them to be given as an array of quoted strings
        INPUT=$(cat)

        # '-c' should cause jq to put each string on a single line, i.e. using
        # '\n' for new lines.
        while read -r STR
        do
          # Unwrap the given quoted string, pass through typeconvert, then
          # replace all 'Integer' types (used for monomorphising) with 'nat'
          echo "$STR" | jq -r '.'                   |
                        runhaskell "${typeconvert}" |
                        sed -e 's/Integer/nat/g'    |
                        jq -Rs '.'
        done < <(echo "$INPUT" | jq -c '.[]') | jq -s '.'
      '';
    }
    ''
      makeWrapper "$raw" "$out" \
        --prefix PATH : "${jq}/bin" \
        --prefix PATH : "${haskellPackages.ghcWithPackages (h: [
                             (hackage h "haskell-src-exts-1.19.1" {
                               "pretty-show" = hackage h "pretty-show-1.6.10" {};
                             })
                           ])}/bin"
    '';

  make_isacosy_theory = haskell-tip: runCommand "make_isacosy_theory"
    {
      buildInputs = [ makeWrapper ];
      raw         = writeScript "make_isacosy_theory-raw" ''
        #!/usr/bin/env bash
        ln -s "${isabelle-tip}"/A.thy ./A.thy
        cp "${isacosy-template}" "ISACOSY.thy"

        NAMES=$(cat)
        TYPES=$(echo "$NAMES" | "${haskellTypesOf haskell-tip.annotatedAsts}" |
                                "${haskellToIsabelleTypes}")
        DATA='[]'
        while read -r PAIR
        do
          N=$(echo "$PAIR" | cut -f1)
          T=$(echo "$PAIR" | cut -f2)
          DATA=$(echo "$DATA" | jq --arg name "$N" --argjson type "$T" \
                                   '. + [{"name": $name, "type": $type}]')
        done < <(paste <(echo "$NAMES") <(echo "$TYPES" | jq -c '.[]'))

        echo "DATA: $DATA" 1>&2
        echo "FIXME: Replace template contents with names and types from stdin" 1>&2

        # Example: @{term "Groups.plus_class.plus :: nat => nat => nat"}
        FUNCS=$(echo "$DATA" |
                jq -r 'map("@{term \"A." + .name + " :: " + .type + "\"}") |
                       reduce .[] as $item (""; . + ", " + $item)' |
                sed -e 's/^, //g')

        sed -i ISACOSY.thy \
            -e "s/(\*TEMPLATE_REPLACE_ME_WITH_FUNCTIONS\*)/$FUNCS/"

        # Example: @{thms "Nat.plus_nat.simps"}
        THMS=$(echo "$DATA" |
               jq -r 'map("@{thms \"A." + .name + ".simps\"}") |
                      reduce .[] as $item (""; . + ", " + $item)' |
               sed -e 's/^, //g')
        sed -i ISACOSY.thy \
            -e "s/(\*TEMPLATE_REPLACE_ME_WITH_DEFINITIONS\*)/$THMS/"

        echo "$PWD/ISACOSY.thy"
      '';
    }
    ''
      mkdir -p "$out/bin"
      makeWrapper "$raw" "$out/bin/make_isacosy_theory" \
                         --prefix PATH : "${jq}/bin"
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

  isacosy-nat =
    let theoryName = "IsaCoSyNat";
        theory = writeScript "${theoryName}.thy" ''
          theory ${theoryName}

          imports Main IsaP IsaCoSy Orderings Set Pure List
          begin

          ML {*
            val functions = map Term.dest_Const
              [@{term "Groups.plus_class.plus :: nat => nat => nat"},
               @{term "Groups.minus_class.minus :: nat => nat => nat"},
               @{term "Groups.times_class.times :: nat => nat => nat"}];

            val def_thrms = [@{thms "Nat.plus_nat.simps"},
                             @{thms "Nat.minus_nat.simps"},
                             @{thms "Nat.times_nat.simps"}];

            val fundefs = functions ~~ def_thrms;

            (* set constraint params *)
            val cparams0 = ConstraintParams.empty
              |> ThyConstraintParams.add_eq        @{context}
              |> ThyConstraintParams.add_datatype' @{context} @{typ "nat"}
              |>    ConstraintParams.add_consts    functions;

            (* Perform the exploration *)
            val (_, nw_ctxt) = SynthInterface.thm_synth
                               SynthInterface.rippling_prover
                               SynthInterface.quickcheck
                               SynthInterface.wave_rule_config
                               SynthInterface.var_allowed_in_lhs
                               { max_size = 8,
                                 min_size = 3,
                                 max_vars = 3,
                                 max_nesting = SOME 2 }
                               (Constant.mk "HOL.eq")
                               (cparams0, @{context});

            (* Extract the results *)
            val result_context = SynthOutput.Ctxt.get nw_ctxt;

            val found_conjectures = map (Trm.pretty nw_ctxt)
                                        (SynthOutput.get_conjs result_context);

            val found_theorems    = map (fn (_, thm) =>
                                          Trm.pretty nw_ctxt
                                            (Trm.change_frees_to_fresh_vars
                                              (Thm.full_prop_of thm)))
                                        (SynthOutput.get_thms result_context);

            (* Write output, delimited to ease chopping off Isabelle noise *)
            PolyML.print (Pretty.output NONE (Pretty.list "[" "]"
              ([Pretty.str "BEGIN OUTPUT"] @
               found_theorems              @
               found_conjectures           @
               [Pretty.str "END OUTPUT"])));
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

          buildInputs  = [ perl isaplanner moreutils gnugrep ];
          buildCommand = ''
            source $stdenv/setup
            set -x

            # Theory name must match file name; 'tip' uses the name "A"
            cp "$theory" "${theoryName}.thy"

            mkdir -p "$out"

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

      mkdir -p "$out"
      "${./extract_eqs.sh}" < "$data/output" > "$out/equations"
    '';
  };
}
