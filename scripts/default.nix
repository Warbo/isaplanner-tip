{ bash, coreutils, fail, haskell, haskellPackages, jq, lib, mkBin, python,
  runCommand, withDeps, wrap }:

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

    # We pick GHC 7.10.3 since the 8.x was failing to build Aeson due to missing
    # constructors in GHC.Generics.
    paths = [ (haskell.packages.ghc7103.ghcWithPackages (h: [
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

  isabelleTypeArgs =
    with rec {
      untested = wrap {
        name  = "isabelleTypeArgs";
        file  = ./IsabelleTypeArgs.hs;
        paths = [ (haskellPackages.ghcWithPackages (h: [
          h.aeson h.bytestring h.parsec h.QuickCheck
        ])) ];
      };

      test = runCommand "isabelleTypeArgs-tests"
        {
          buildInputs = [ fail jq ];
          script      = untested;
        }
        ''
          set -e
          set -o pipefail

          RUN_TESTS=1 "$script" || fail "Self-test failed"

          function check {
             GOT=$(echo    "$1" | "$script" | jq 'sort')
            WANT=$(echo -e "$2"             | jq 'sort')

            # Check every element of GOT is in WANT and vice versa

            jq -e -n --argjson got "$GOT" --argjson want "$WANT" \
               '$got == $want' || fail "'$1' gave '$GOT' not '$WANT'"
          }

          # These will accumulate the inputs and outputs, so we can check JSON
          # containing multiple types
          IS='[]'
          OS='[]'

          for EXAMPLE in \
            '["nat"]                           	["nat"]'                       \
            '["nat => nat"]                    	["nat"]'                       \
            '["nat => nat => nat"]             	["nat"]'                       \
            '["nat => int => bool"]            	["nat", "int", "bool"]'        \
            '["(nat => int) => bool"]          	["nat", "int", "bool"]'        \
            '["z => (z y x => y w)"]           	["z", "z y x", "y", "y w"]'    \
            '["(a A, a) B => (a A, a) B"]      	["(a A, a) B", "a A", "a"]'    \
            '["(a => (b => c => d)) => e => f"]	["a", "b", "c", "d", "e", "f"]'
          do
            I=$(echo "$EXAMPLE" | cut -f1)
            O=$(echo "$EXAMPLE" | cut -f2)

            check "$I" "$O"

            # Append this example to our accumulating input/output arrays
            IS=$(echo "$IS" | jq --argjson i "$I" '. + $i')
            OS=$(echo "$OS" | jq --argjson o "$O" '. + $o')
          done

          # Check that multiple inputs work
          check "$IS" "$OS"

          mkdir "$out"
        '';
    };
    withDeps [ test ] untested;

  listUndefined = { te-benchmark }: wrap {
    name  = "listUndefined";
    paths = [ coreutils (envFrom te-benchmark) fail ];
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
