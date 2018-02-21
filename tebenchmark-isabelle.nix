# TEBenchmark converted to an Isabelle theory
{ bash, callPackage, fetchFromGitHub, gcc, getBenchmarkTypes, getPreprocessed,
  haskell, isaplanner, jq, latestGit, nonExhaustiveScraper, perl, runCabal2nix,
  runCommand, stdenv, stripConstructorsDestructors, writeScript }:

rec {
  te-benchmark-src = latestGit {
    url    = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
    stable = {
      rev    = "e993b2f";
      sha256 = "0zwd2bynj645b5frvmkb9mc4fhvvqv25ls6sh56wwaq69a5k76rq";
    };
  };

  te-benchmark = callPackage "${te-benchmark-src}" {
    haskellPackages = haskell.packages.ghc802;
  };

  haskellPackages = te-benchmark.patchedHaskellPackages;

  withParser = runCommand "mk-parser"
    {
      buildInputs = [ bash haskellPackages.BNFC gcc ];
      tipSrc      = latestGit {
        url    = http://chriswarbo.net/git/tip-tools.git;
        stable = {
          rev    = "2b8ed14";
          sha256 = "1nmja63agidjfspppgsrd6fv7fzc3vadw528cwj9vqajs9sgnla5";
        };
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

  tip-lib = runCabal2nix { url = parserSrc; };

  tebenchmark-isabelle = make-tebenchmark-isabelle { inherit te-benchmark; };

  # Note: to comply with Isabelle's naming conventions, the output of this
  # derivation should be copied to a file called 'A.thy' before importing.
  make-tebenchmark-isabelle = args: runCommand "tebenchmark-isabelle"
    { data = tip-convert args; }
    ''
      ln -s "$data/A.thy" "$out"
    '';

  find-undefined-cases = args: runCommand "undefined-cases"
    {
      inherit nonExhaustiveScraper;
      buildInputs = [ haskellPackages.ghc ];
      data        = tip-convert args;
    }
    ''
      cp "$data/A.hs" ./A.hs
      ghc -fwarn-incomplete-patterns A.hs 2>&1 |
        "$nonExhaustiveScraper" > "$out"
    '';

  tip-convert = { te-benchmark }: runCommand
    "tip-conversion"
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
      "$getPreprocessed" > pp
      tip --isabelle < pp > A.thy

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

      mkdir -p "$out"
      cp A.thy "$out"/

      tip --haskell < pp > "$out/A.hs"
    '';

  tebenchmark-data = make-tebenchmark-data { inherit te-benchmark; };

  make-tebenchmark-data = { te-benchmark }:
    stdenv.mkDerivation {
      name    = "tebenchmark-data.json";
      builder = getBenchmarkTypes {
        inherit te-benchmark;
        tebenchmark-isabelle = make-tebenchmark-isabelle {
          inherit te-benchmark;
        };
      };
    };

  # IsaCoSy will include constructors in its exploration, even if they're not
  # in the sample. This artificially lowers the precision, which is unfair. To
  # counteract this, the following script can be used to discard problematic
  # equations before they reach the precision/recall stage.
  handleConstructors = stripConstructorsDestructors { inherit te-benchmark; };
}
