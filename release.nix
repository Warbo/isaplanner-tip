let pkgs = import <nixpkgs> {};
 in with pkgs; rec {

  polyml = pkgs.polyml.overrideDerivation (old: {
    name = "polyml-5.5.2";
    src  = "${isabelle2015}/contrib/polyml-5.5.2-3/src";
    # src = fetchFromGitHub {
    #   owner = "polyml";
    #   repo  = "polyml";
    #   rev   = "88a7241"; # fixes-5.5.2
    #   sha256 = "0z34l4rcm9nxpvm2qb026pv51d0s98kkw88pnac7gqjam93c7fv2";
    # };
    #installPhase = ''
    #  find .
    #'';
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

    #libPath = [
    #  gmp
    #  jdk
    #  (runCommand "jli-lib" {
    #                inherit jdk;
    #                SYS = {   i686-linux = "i386";
    #                        x86_64-linux = "amd64";
    #                      }."${builtins.currentSystem}";
    #              }
    #              ''
    #     mkdir -p "$out"
    #     cp -r "$jdk/lib/openjdk/lib/$SYS" "$out/lib"
    #   '')
    #];

    #SYS = { i686-linux   = "x86-linux";
    #        x86_64-linux = "x86_64-linux"; }."${builtins.currentSystem}";

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
      unset ORIG

      echo "Patching with '$postPatchPath'" 1>&2
      . "$postPatchPath"
    '';

    #configurePhase = ''
    #'';
    buildPhase = ''
      set -e
      ISABELLE_DIR="$PWD"
      ISABELLE_JDK_HOME=$(dirname "$(dirname "$(command -v javac)")")

      export ISABELLE_JDK_HOME
      HOME=$(dirname "$PWD")
      export HOME

      echo "Patching hard-coded executables" 1>&2
      INTERP=$(cat $NIX_CC/nix-support/dynamic-linker)
      #while read -r F
      #do
      #  if file "$F" | grep "interpreter /lib/ld-linux.so.2" > /dev/null
      #  then
      #    patchelf --set-interpreter "$INTERP" --set-rpath "$libPath" "$F"
      #  fi
      #done < <(find . -type f)

      cd contrib/IsaPlanner
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
