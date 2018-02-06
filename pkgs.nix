# Nixpkgs sets, augmented with some custom configuration
with rec {
  # Mixes unstable <nixpkgs> and stable config; only use as way to get fully
  # stable and unstable versions.
  pkgsWithCfg = import <nixpkgs> { config = import "${stableCfg}/config.nix"; };

  stableCfg =
    with rec {
      rev = "044d89400eb8e5b0e9123a88b70e4e8b688ab50d";
      url = "https://github.com/Warbo/nix-config/archive/${rev}.tar.gz";
    };
    builtins.fetchTarball url;

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
