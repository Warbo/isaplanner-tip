# Isabelle with Isaplanner support
{ bash, fetchFromGitHub, fetchgit, fetchurl, file, isabelle, jdk, makeWrapper,
  nettools, perl, polyml, stdenv, writeScript }@args:

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
}
