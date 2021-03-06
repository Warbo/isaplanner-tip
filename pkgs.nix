# Nixpkgs sets, augmented with some custom configuration
with builtins;
with rec {
  # If we have nix-config use it, otherwise fetch one
  options =
    with tryEval <nix-config>;
    if success
       then { config = import "${value}/stable.nix"; f = "fetchGitHashless"; }
       else { config = {};                           f = "fetchgit";         };

  pkgsWithCfg = import <nixpkgs> { config = import "${stableCfg}/config.nix"; };

  stableCfg =
    getAttr options.f
            (import <nixpkgs> { inherit (options) config; })
            {
              url    = http://chriswarbo.net/git/nix-config.git;
              rev    = "044d894";
              sha256 = "0y873r2nfw9qazdl555kwj2072n532pnv5iwp3nyck614qncvvzl";
            };

  unstableCfg = with tryEval <nix-config>;
                if success
                   then import "${value}/unstable.nix"
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
    config = unstableCfg;
  };
}
