{ latestGit }:

with builtins;
rec {
  get-haskell-te = rev: rec {
    haskell-te     = import "${haskell-te-src}/nix-support" {};
    haskell-te-src = latestGit {
      url    = http://chriswarbo.net/git/haskell-te.git;
      stable = {
        inherit rev;
        sha256 = getAttr rev haskell-te-hashes;
      };
    };
  };

  # Known versions of haskell-te, e.g. those used for particular benchmark runs
  haskell-te-hashes = {
    "a66be4f" = "0kbp4f40bhwxxzzq3dx4g4294s7if4bxhgxiy7gw9y7acfbpc7js";
  };

  inherit (get-haskell-te "a66be4f") haskell-te haskell-te-src;
}
