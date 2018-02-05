# Isabelle with Isaplanner support
{ bash, fail, fetchFromGitHub, fetchgit, fetchurl, file, isabelle, jdk,
  makeWrapper, nettools, perl, polyml, stdenv, wrap, writeScript }@args:

rec {
  # The ML system Isabelle is written in
  polyml = args.polyml.overrideDerivation (old: {
    name = "polyml-5.5.2";
    src  = "${isabelle2015}/contrib/polyml-5.5.2-3/src";
  });

  # This Isabelle version is known to work with IsaCoSy
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

  # Dependency of Isaplanner
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

  # Isabelle with Isaplanner
  isaplanner = stdenv.mkDerivation {
    name = "isaplanner";
    src  = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "IsaPlanner";
      rev    = "66d52b3";
      sha256 = "1ym7kwi0j4i8gy90dyx8h02kjz9h2j70ivw4ggw328p82n6n23x2";
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
      # quick_and_dirty mode allows proofs to be skipped with "sorry".
      isabelle console -n -d "$ISAPLANNER_DIR"     \
                          -l HOL-IsaPlannerSession \
                          -o quick_and_dirty "$@"
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

  show_theory = wrap {
    name   = "show_theory";
    paths  = [ bash fail isaplanner ];
    vars   = {
      msg = ''
        Starting Isabelle. Please be patient, as it can take a while. First we
        load the theory, then we print it. Note that this is not an interactive
        command, so ignore prompts like 'ML> '; they're just line noise from
        Isabelle.
      '';
      hint = ''
        NOTE:
        Isabelle is very particular about theory files. We must give it files
        with names like 'foo.thy', where 'foo' matches the name appearing in the
        'theory' line of the file (e.g. 'theory foo').

        Note that symlinks will work. For example, if you have a theory called
        'foo' in a file '/path/to/bar' then you can appease Isabelle by making a
        symlink like 'foo.thy' -> '/path/to/bar' and running with 'foo.thy' as
        the input filename.
      '';
    };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Loads the given Isabelle theory file and dumps out its contents

      [[ -n "$1" ]] || fail "No .thy file given"
      [[ -e "$1" ]] || fail "Couldn't find theory file '$1'"
      echo "$1" | grep '.thy${"$"}' > /dev/null || {
        echo "$hint" 1>&2
        fail "ERROR: Theory filename '$1' doesn't end in '.thy'"
      }

      NAME=$(basename "$1" .thy)
       DIR=$(dirname  "$1")

      function makeCommands {
        # Load the theory; populates some state (dependency graphs, etc.)
        echo "use_thy \"$NAME\";"

        # Look up the theory and print it
        echo "Proof_Display.print_theory (Thy_Info.get_theory \"$NAME\");"
      }

      pushd "$DIR" > /dev/null
        echo "$msg" 1>&2
        makeCommands | isaplanner
      popd > /dev/null
    '';
  };
}
