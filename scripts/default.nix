{ bash, coreutils, fail, haskell, haskellPackages, jq, lib, mkBin, nixpkgs1709,
  runCommand, withDeps, wrap, writeScript }:

with builtins;
with lib;
with {
  # Prior versions suffer from https://github.com/NixOS/nixpkgs/pull/23600
  inherit (nixpkgs1709) python;
};
rec {
  cutoff-timer = { runners, timeout_secs }: wrap {
    name  = "cutoff-timer";
    file  = ./cutoff-timer.py;
    paths = [ (python.withPackages (p: [ p.subprocess32 ])) ];
    vars  = {
      runners      = toJSON runners;
      timeout_secs = toString timeout_secs;
    };
  };

  eqsToJson =
    with rec {
      untested = wrap {
        name = "eqsToJson.hs";
        file = ./eqsToJson.hs;

        # We pick GHC 7.10.3 since the 8.x was failing to build Aeson due to
        # missing constructors in GHC.Generics.
        paths = [ (haskell.packages.ghc7103.ghcWithPackages (h: [
          h.aeson h.parsec h.QuickCheck
        ])) ];
      };

      test = runCommand "eqsToJson-test"
        {
          inherit untested;
          RUN_TESTS = "1";
        }
        ''
          "$untested" || exit 1
          mkdir "$out"
        '';
    };
    withDeps [ test ] untested;

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
    paths = [ te-benchmark.env ];
    vars  = te-benchmark.cache // {
      FIXES       = ./fixes.json;
      PLTCOLLECTS = ":${te-benchmark.scripts}";
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

  nonExhaustiveScraper =
    with rec {
      untested = wrap {
        name  = "nonExhaustiveScraper";
        paths = [ (haskellPackages.ghcWithPackages (h: [
          h.aeson h.bytestring h.parsec h.QuickCheck
        ])) ];
        file  = ./NonExhaustiveScraper.hs;
      };

      test = runCommand "non-exhaustive-test"
        {
          inherit untested;
          buildInputs = [ fail jq ];
          example     = writeScript "non-exhaustive-example" ''
            [1 of 1] Compiling A                ( A.hs, A.o )

            A.hs:6044:1: Warning:
                Pattern match(es) are non-exhaustive
                In an equation for `global6465':
                    Patterns not matched:
                        Global6772

            A.hs:6244:1: Warning:
                Pattern match(es) are non-exhaustive
                In an equation for `global7374':
                    Patterns not matched:
                        Global7469 _
                                   _
                        Global7032 _

          '';
        }
        ''
          set -e
          RUN_TESTS=1 "$untested"

          "$untested" < "$example" > got.json
          for QUERY in 'type | . == "object"'                     \
                       'keys | length | . == 2'                   \
                       'has("global6465")'                        \
                       'has("global7374")'                        \
                       '.global6465 | keys | . == ["Global6772"]' \
                       '.global6465 | .Global6772 | . == 0'       \
                       '.global7374 | keys | length | . == 2'     \
                       '.global7374 | has("Global7469")'          \
                       '.global7374 | has("Global7032")'          \
                       '.global7374 | .Global7469 | . == 2'       \
                       '.global7374 | .Global7032 | . == 1'
          do
            jq -e "$QUERY" < got.json > /dev/null || {
              cat got.json 1>&2
              fail "\njq query '$QUERY' failed"
            }
          done

          mkdir "$out"
        '';
    };
    withDeps [ test ] untested;

  stripConstructorsDestructors = { te-benchmark }: wrap {
    name  = "stripConstructorsDestructors";
    file  = ./stripConstructorsDestructors.rkt;
    paths = [ te-benchmark.env ];
    vars  = te-benchmark.cache // {
      PLTCOLLECTS = ":${te-benchmark.scripts}";
    };
  };
}
