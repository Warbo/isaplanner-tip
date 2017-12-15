{ bash, haskellPackages, jq, mkBin, python, runCommand, withDeps, wrap }:

rec {
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
    paths = [ (haskellPackages.ghcWithPackages (h: [ h.parsec ])) ];
    file  = ./IsabelleTypeArgs.hs;
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
