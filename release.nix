let pkgs = import <nixpkgs> {};
 in with pkgs; rec {

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

    buildInputs = let te-benchmark-src = fetchgit {
                        url    = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
                        rev    = "b668e34";
                        sha256 = "1vabsxdli29i8mxrn61n9yqb5psysc8xq1g7vz13lfymv2a0ypbd";
                      };
                      te-benchmark = callPackage "${te-benchmark-src}" {
                        haskellPackages = haskell.packages.ghc7103;
                      };
                   in [ te-benchmark.tip-benchmark-smtlib ];

    buildCommand = ''
      source $stdenv/setup

      F=$(completeTipSig)

      echo "Converting smtlib data in '$F' into isabelle code" 1>&2
      tip --isabelle < "$F" > "$out"
    '';
  };
}

# # Install the necessary base packages for Isabelle/TheoryMine theorem
# # generation
# RUN apt-get update && apt-get install -y \
#   g++ \
#   git \
#   make \
#   openjdk-6-jre \
#   subversion \
#   wget \
#   nano

# # Install additional packages for latex/certifictae/image generation
# RUN apt-get install -y \
#   imagemagick \
#   curl \
#   texlive-latex-extra \
#   latex-cjk-all
