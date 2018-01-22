# Nixpkgs sets, augmented with some custom configuration
with rec {
  # Mixes unstable <nixpkgs> and stable config; only use as way to get fully
  # stable and unstable versions.
  pkgsWithCfg = import <nixpkgs> { config = import "${stableCfg}/config.nix"; };

  stableCfg   = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "044d894";
    sha256 = "0s9jw6mna8qpl5s5phd9qx1im2pcclr43fmbm40i72v95nn8q8xh";
  };

  unstableCfg =
    with builtins.tryEval <nixpkgs>;
    if success
       then value
       else pkgsWithCfg.latestNixCfg;
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
