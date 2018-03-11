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
    "3c15e23" = "1y9ak8qwwa5g7ha2n2d26rbavgcv3y19s6qkk127wc0dip70z806";
    "1c6ae74" = "0g8yhhvshav6hgdpkra3pz2avlyhh11rl1x56zakf370hawd78n2";
  };

  inherit (get-haskell-te "1c6ae74") haskell-te haskell-te-src;
}
