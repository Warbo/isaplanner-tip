with import <nixpkgs> {};

rec {

  isabelle = stdenv.mkDerivation {
    name = "isabelle";
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
    inherit isabelle;
    installPhase = ''
      PLIB="$PWD"
      cd ..
      cp -a "$isabelle" ./isabelle
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
    buildInputs = [ jdk perl ];

    configurePhase = ''
      PLANNER="$PWD"
      cd ..
      PAR="$PWD"

      cp -a "$isaplib" ./isabelle
      chmod +w -R ./isabelle
      cp -a "$PLANNER" ./isabelle/contrib/IsaPlanner
    '';
    buildPhase = ''
      ISABELLE_DIR="$PAR/isabelle"
      ISABELLE_JDK_HOME=$(dirname "$(dirname "$(command -v javac)")")

      export ISABELLE_JDK_HOME

      find .

      cd ./isabelle/contrib/IsaPlanner
      ../../bin/isabelle build -d . HOL-IsaPlannerSession
      ../../bin/isabelle build -d . IsaPlanner-Test
    '';
    installPhase = ''
      cd "$PLANNER"
      cd ..
      cp -a ./isabelle "$out"
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
