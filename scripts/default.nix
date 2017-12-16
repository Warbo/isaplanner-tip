{ bash, haskell, haskellPackages, jq, mkBin, python, runCommand, withDeps, wrap }:

rec {
  eqsToJson = wrap {
    name = "eqsToJson.hs";
    file = ./eqsToJson.hs;

    # We pick GHC 8.0.1 since the default package set was failing to build Aeson
    # due to missing fail and semigroups dependencies.
    paths = [ (haskell.packages.ghc801.ghcWithPackages (h: [
      h.aeson h.parsec
    ])) ];
  };

  extractEqs = wrap {
    name  = "extractEqs.py";
    file  = ./extractEqs.py;
    paths = [ python ];
  };

  getPreprocessed = { te-benchmark }: wrap {
    name  = "preprocess.sh";
    file  = ./preprocess.sh;
    paths = [ bash jq ];
    vars  = {
      FIXES  = ./fixes.json;
      smtlib = te-benchmark.tip-benchmark-smtlib;
    };
  };

  getBenchmarkTypes = { te-benchmark, te-benchmark-src, tebenchmark-isabelle }:
    wrap {
      name  = "getBenchmarkTypes.rkt";
      file  = ./getBenchmarkTypes.rkt;
      paths = [ te-benchmark.env ];
      vars  = te-benchmark.cache // {
        FIXES       = ./fixes.json;
        PLTCOLLECTS = ":${te-benchmark-src}/scripts";
        smtlib      = te-benchmark.tip-benchmark-smtlib;
        tebIsabelle = tebenchmark-isabelle;
      };
    };

  isabelleTypeArgs-untested = wrap {
    name  = "isabelleTypeArgs";
    file  = ./IsabelleTypeArgs.hs;
    paths = [ (haskellPackages.ghcWithPackages (h: [ h.parsec ])) ];
  };

  isabelleTypeArgs = withDeps
    [
      (runCommand "isabelleTypeArgs-tests"
        { script = isabelleTypeArgs-untested; }
        ''
          set -e
          set -o pipefail

          function check {
             GOT=$(echo "$1" | "$script")
            WANT=$(echo -e "$2")

            [[ "x$GOT" = "x$WANT" ]] || {
              echo "Got '$GOT' for '$1' instead of '$WANT'" 1>&2
              exit 1
            }
          }

          check "nat"                  ""
          check "nat => nat"           "nat"
          check "nat => nat => nat"    "nat"
          check "nat => int => bool"   "nat\nint"
          check "(nat => int) => bool" "nat => int\nnat"

          check "(a => (b => c => d) => e) => f => g" \
                 "a => (b => c => d) => e\nf\na\nb => c => d\nb\nc"

          mkdir "$out"
        '')
    ]
    isabelleTypeArgs-untested;
}
