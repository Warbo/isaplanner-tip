# Nixpkgs sets, augmented with some custom configuration
with rec {
  # Mixes unstable <nixpkgs> and stable config; only use as way to get fully
  # stable and unstable versions.
  pkgsWithCfg = import <nixpkgs> { config = import "${stableCfg}/config.nix"; };

  stableCfg   = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "76d441a";
    sha256 = "047vqfyb7qbl49hyi93vfz5dkqpz89jjscs1w5kc29hn6881v0w8";
  };

  unstableCfg = pkgsWithCfg.latestNixCfg;
};
{
  # A known-good version of our configuration; this is "production"
  stable = import pkgsWithCfg.repo1609 {
    config = import "${stableCfg}/stable.nix";
  };

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstable = import <nixpkgs> {
    config = import "${unstableCfg}/unstable.nix";
  };
}
