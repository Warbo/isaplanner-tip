# TEBenchmark converted to an Isabelle theory
{ bash, callPackage, fetchFromGitHub, fetchgit, gcc, getBenchmarkTypes,
  getPreprocessed, haskell, isaplanner, jq, perl, runCommand, stdenv,
  writeScript }:

rec {
  te-benchmark-src = fetchgit {
    url    = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
    rev    = "ccf838d";
    sha256 = "1isbzv29903fh3m1sikj6gyaylq6wcw042wxna1g6k8wnlac9xjb";
  };

  te-benchmark = callPackage "${te-benchmark-src}" {
    haskellPackages = haskell.packages.ghc802;
  };

  haskellPackages = te-benchmark.patchedHaskellPackages;

  withParser = runCommand "mk-parser"
    {
      buildInputs = [ bash haskellPackages.BNFC gcc ];
      tipSrc      = fetchgit {
        url    = http://chriswarbo.net/git/tip-tools.git;
        rev    = "2b8ed14";
        sha256 = "1nmja63agidjfspppgsrd6fv7fzc3vadw528cwj9vqajs9sgnla5";
      };
    }
    ''
      cp -r "$tipSrc" ./tip
      chmod -R +w ./tip
      pushd ./tip
        bash make_parser.sh
        ln -s $(gcc --print-file-name=libstdc++.so)
        pushd tip-lib
         ln -s $(gcc --print-file-name=libstdc++.so)
        popd
      popd
      cp -r ./tip "$out"
    '';

  parserSrc = runCommand "tip-lib-with-parser" { inherit withParser; } ''
    source $stdenv/setup

    cp -a "$withParser/tip-lib" ./tip-lib
    chmod +w -R ./tip-lib

    for F in src/Tip/Pass/Pipeline.hs src/Tip/Passes.hs executable/Main.hs
    do
      echo "Patching $F" 1>&2
      sed -e 's/\(import Options.Applicative\)/\1\nimport Data.Monoid ((<>))/' \
          < "./tip-lib/$F" > temp
      mv temp "./tip-lib/$F"
    done

    cp -a ./tip-lib "$out"
  '';

  nixpkgs1703 = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "1360efe";
    sha256 = "0y4db7akf7h03kpqr145qzzkbayamn9fkzxhx04abq5jdkxxjv0d";
  }) { config = {}; };

  tip-lib = nixpkgs1703.haskellPackages.haskellSrc2nix {
    name = "tip-lib";
    src  = parserSrc;
  };

  # Note: to comply with Isabelle's naming conventions, the output of this
  # derivation should be copied to a file called 'A.thy' before importing.
  tebenchmark-isabelle = runCommand "tebenchmark-isabelle"
    {
      buildInputs = [
        (haskellPackages.ghcWithPackages (h: [ (h.callPackage tip-lib {}) ]))
        isaplanner
      ];
      getPreprocessed = getPreprocessed { inherit te-benchmark; };
    }
    ''
      set -e
      set -o pipefail

      echo "Converting smtlib data into isabelle code" 1>&2
      "$getPreprocessed" | tip --isabelle > A.thy

      echo "Testing" 1>&2
      OUTPUT=$(echo 'use_thy "A";' | isaplanner)

      if echo "$OUTPUT" |  grep -i -C 10 'error' > /dev/null
      then
         echo "Fail: Errors found in generated theory" 1>&2
         echo "$OUTPUT" 1>&2
         cp -v "A.thy" /tmp/A.thy
         exit 1
      fi
      echo "Passed" 1>&2

      cp A.thy "$out"
    '';

  tebenchmark-data = runCommand "tebenchmark-data.json"
    (te-benchmark.cache  // {
      buildInputs = [ te-benchmark.env ];
      FIXES       = ./fixes.json;
      PLTCOLLECTS = ":${te-benchmark-src}/scripts";
      script      = ./tebenchmark-data.rkt;
      smtlib      = te-benchmark.tip-benchmark-smtlib;
      tebIsabelle = tebenchmark-isabelle;
    })
    ''racket "$script" > "$out"'';


  # namesFile contains newline-separated names to include in a sample
  tebenchmark-sample = { namesFile }: abort "FIXME";
}
