with rec {
  bootPkgs = call ((import <nixpkgs> {}).fetchgit {
    url    = cfgUrl;
    rev    = "be310e2";
    sha256 = "166lzyxyh2d2ivm6bf3nrb55p00cf7q0pv1gskj8crxsx4ym8w2h";
  });

  # Use bootPkgs.latestGit to get the latest nix-config
  pkgs   = call (bootPkgs.latestGit { url = cfgUrl; });
  call   = f: import "${f}" {};
  cfgUrl = http://chriswarbo.net/git/nix-config.git;
};
with pkgs;

let these = import ./. { inherit pkgs; };
    defs = rec {
      inherit these;

      haskell-te-src = latestGit {
        url = "http://chriswarbo.net/git/haskell-te.git";
      };

      haskell-te = (import "${haskell-te-src}").allDefs;

      tip-benchmarks = latestGit {
        url = "http://chriswarbo.net/git/theory-exploration-benchmarks.git";
      };
    };
in with these; with defs; rec {
  debug = defs;

  haskellTypesOfTip = runCommand "haskell-types-of-tip"
    {
      typesof   = haskellTypesOf haskell-te.tipBenchmarks.annotatedAsts;
      annotated = haskell-te.tipBenchmarks.annotatedAsts;
    }
    ''
      set -e
      NAMES=$(jq -r '.[] | .name' < "$annotated" | shuf | head -n10)
      TYPES=$(echo "$NAMES" | "$typesof")
      while read -r NAME
      do
        TYPE=$(jq -r --arg name "$NAME" \
                  'map(select(.name == $name)) | .[] | .type' < "$annotated")
        echo "$TYPES" | grep -Fx '$TYPE' || {
          echo "Didn't find type of '$NAME' ($TYPE) in output:" 1>&2
          echo "$TYPES" 1>&2
          exit 1
        }
      done < <(echo "$NAMES")
    '';
}
