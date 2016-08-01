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
    buildInputs = [ isaplib ];
  };
}
#     # Install isaplib for Isabelle-2015
#     RUN git clone --branch Isabelle-2015 https://github.com/iislucas/isaplib.git \
#         /usr/local/Isabelle2015/contrib/isaplib

#         # Copy the local directory as the IsaPlanner directory for Isabelle2009-2
#         COPY . /usr/local/Isabelle2015/contrib/IsaPlanner

#         RUN cd /usr/local/Isabelle2015/contrib/IsaPlanner && \
#           /usr/local/Isabelle2015/bin/isabelle build -d . HOL-IsaPlannerSession && \
#             /usr/local/Isabelle2015/bin/isabelle build -d . IsaPlanner-Test


# # Docker file for TheoryMine

# FROM ubuntu:14.04
# MAINTAINER Lucas Dixon <lucas.dixon@gmail.com>

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

# # Install Isabelle-2015
# RUN curl http://isabelle.in.tum.de/website-Isabelle2015/dist/Isabelle2015_linux.tar.gz \
#   -o /usr/local/Isabelle2015_linux.tar.gz

# RUN tar zxvf /usr/local/Isabelle2015_linux.tar.gz -C /usr/local/

# # This is just a line to edit to make the below force to be re-run.
# RUN echo "id:1"

# # Install isaplib for Isabelle-2015
# RUN git clone --branch Isabelle-2015 https://github.com/iislucas/isaplib.git \
#     /usr/local/Isabelle2015/contrib/isaplib

# # Copy the local directory as the IsaPlanner directory for Isabelle2009-2
# COPY . /usr/local/Isabelle2015/contrib/IsaPlanner

# RUN cd /usr/local/Isabelle2015/contrib/IsaPlanner && \
#   /usr/local/Isabelle2015/bin/isabelle build -d . HOL-IsaPlannerSession && \
#   /usr/local/Isabelle2015/bin/isabelle build -d . IsaPlanner-Test


#     propagatedBuildInputs = [ docker ];

#     buildPhase = ''
#       docker build -t theorymine/isaplanner:2015.0.2 .
#     '';

#     installPhase = ''
#       mkdir -p "$out/bin"
#       cp "${isaplannerRun}" "$out/bin/isaplanner-run"
#     '';

#     isaplannerRun = writeScript "isaplanner-run" ''
#       docker run -i -t theorymine/isaplanner:2015.0.2 /bin/bash
#     '';
#   };
# }
