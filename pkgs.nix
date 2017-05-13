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
    rev    = "f07885a";
    sha256 = "190cxpk2r42zyhj972ndzsr81r9p9dashfx9dgg1fgshyp6hz8j6";
  });

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstable = call (stable.latestGit { inherit url; });
}
