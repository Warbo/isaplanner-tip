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
    rev    = "1f455dd";
    sha256 = "1k39ki6h8mciama2k7g5hd2niam5lj1fkyp0ffmn5ncbjfniavjp";
  });

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstable = call (stable.latestGit { inherit url; });
}
