# TEBenchmark converted to an Isabelle theory
{ bash, callPackage, fetchFromGitHub, fetchgit, gcc, haskell, isaplanner, jq,
  perl, runCommand }:

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

  tebenchmark-isabelle = runCommand "tebenchmark-isabelle"
    {
      buildInputs  = [ (haskellPackages.ghcWithPackages (h: [
                         (h.callPackage tip-lib {}) ]))
                       isaplanner jq perl ];
      smtdata      = te-benchmark.tip-benchmark-smtlib;
      FIXES        = ./fixes.json;
      preprocess   = ./preprocess.sh;
    }
    ''
      source $stdenv/setup
      set -e
      set -o pipefail

      echo "Converting smtlib data in '$smtdata' into isabelle code" 1>&2
      "$preprocess" < "$smtdata" | tip --isabelle > A.thy

      echo "Testing" 1>&2
      OUTPUT=$(echo 'use_thy "A";' | isaplanner -o quick_and_dirty)

      if echo "$OUTPUT" |  grep -i -C 10 'error' > /dev/null
      then
         echo "Fail: Errors found in generated theory" 1>&2
         echo "$OUTPUT" 1>&2
         cp -v "A.thy" /tmp/A.thy
         exit 1
      fi
      echo "Passed" 1>&2

      mkdir -p "$out"
      mv A.thy "$out/"
    '';
}
