# Nixpkgs sets, augmented with some custom configuration
{ system ? builtins.currentSystem }:
with {
  call = f: import "${f}" {
              pkgFunc = args: import <nixpkgs> ({ inherit system; } // args);
            };
  url  = http://chriswarbo.net/git/nix-config.git;
};
rec {
  # A known-good version of our configuration; this is "production"
  stable = call ((import <nixpkgs> {}).fetchgit {
    inherit url;
    rev    = "521aca2";
    sha256 = "0g7ih65b7lfspbgbiwq6gxa2ffidifbypkw9c2nqy4fqc1lw3vqa";
  });

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstable = call (stable.latestGit { inherit url; });
}
