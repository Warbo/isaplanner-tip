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
    "1dbea17" = "1vmxg7zsk1crk29ng2sd6z3mwrfzsa8amz8b6mz0g82kbw5b2g3f";
    "be30d74" = "0skmiy0riry3jxkz6pk3lf38bqd3lw6gxlh4l6j41ccdm7lwlh55";
    "fc310c1" = "1i3nkqxawnwjfarz96r2cxq0yrsw35cqbgxqi0a7n86arnc8da0k";
  };

  inherit (get-haskell-te "fc310c1") haskell-te haskell-te-src;
}
