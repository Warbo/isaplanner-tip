# Nixpkgs sets, augmented with some custom configuration
{ system ? builtins.currentSystem }:

with {
  call = f: import "${f}" {
    pkgFunc = args: import <nixpkgs> ({ inherit system; } // args);
  };
  url = http://chriswarbo.net/git/nix-config.git;
};
rec {
  # A known-good version of our configuration; this is "production"
  stablePkgs = call ((import <nixpkgs> {}).fetchgit {
    inherit url;
    rev    = "d453c1c";
    sha256 = "1ahf7jzxy462b8bir1909awdhdd5dwyj850fxnfh8mlndjdwarxf";
  });

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstablePkgs = call (stablePkgs.latestGit { inherit url; });
}
