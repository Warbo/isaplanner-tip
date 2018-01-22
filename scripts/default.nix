{ bash, fail, haskell, haskellPackages, jq, lib, mkBin, python, runCommand,
  withDeps, wrap }:

with builtins;
with lib;
with {
  # These need to work for any old revisions in known-samples

  cacheFrom = te-benchmark:
    te-benchmark.cache or (filterAttrs (n: _: hasPrefix "BENCHMARK" n)
                                       te-benchmark.tools);

  envFrom = te-benchmark:
    te-benchmark.env or
    (head (filter (x: x.name == "tip-bench-env")
                  (concatLists (map (n: getAttr n te-benchmark.tools) [
                    "buildInputs" "propagatedNativeBuildInputs"
                    "propagatedBuildInputs" "nativeBuildInputs"
                  ]))));

  scriptsFrom = te-benchmark:
    with rec {
      # Older way: read source of env var derivation
      smtlib = te-benchmark.tip-benchmark-smtlib;
      asVar  = smtlib.BENCHMARKS_FINAL_BENCHMARK_DEFS;

      # Newer way: read PLTCOLLECTS of compiled builder
      bldr   = elemAt smtlib.args 1;
      varNm  = head (attrNames (filterAttrs (n: v: v == "PLTCOLLECTS") bldr));
      valNm  = replaceStrings [ "Names" ] [ "Vals" ] varNm;
      asDep  = removePrefix ":" (getAttr valNm bldr);
    };
    if hasAttr "BENCHMARKS_FINAL_BENCHMARK_DEFS" smtlib
       then asVar.src
       else asDep;
};
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

  getBenchmarkTypes = { te-benchmark, tebenchmark-isabelle }: wrap {
    name  = "getBenchmarkTypes.rkt";
    file  = ./getBenchmarkTypes.rkt;
    paths = [ (envFrom te-benchmark) ];
    vars  = cacheFrom te-benchmark // {
      FIXES       = ./fixes.json;
      PLTCOLLECTS = ":${scriptsFrom te-benchmark}";
      smtlib = te-benchmark.tip-benchmark-smtlib;
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

  listUndefined = { te-benchmark }: wrap {
    name  = "listUndefined";
    paths = [ (envFrom te-benchmark) fail ];
    vars  = cacheFrom te-benchmark // {
      listUndefined = ./listUndefined.rkt;
      tipBenchmark  = te-benchmark.tip-benchmark-smtlib;
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      [[ -e "$namesFile" ]] || fail "No namesFile given"
      SAMPLE=$(cat "$namesFile")
      export SAMPLE
      "$listUndefined" "$@"
    '';
  };

  stripConstructorsDestructors = { te-benchmark }: wrap {
    name  = "stripConstructorsDestructors";
    file  = ./stripConstructorsDestructors.rkt;
    paths = [ (envFrom te-benchmark) ];
    vars  = cacheFrom te-benchmark // {
      PLTCOLLECTS = ":${scriptsFrom te-benchmark}";
    };
  };
}
