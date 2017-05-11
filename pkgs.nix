# Nixpkgs sets, augmented with some custom configuration
with {
  call = f: import "${f}" {};
  url  = http://chriswarbo.net/git/nix-config.git;
};
rec {
  # A known-good version of our configuration; this is "production"
  stable = call ((import <nixpkgs> {}).fetchgit {
    inherit url;
    rev    = "409d7af";
    sha256 = "0999sz8dvlh6q39a2hjkhabflbjqg3a1qj6vlqml6a1aac21syx2";
  });

  # Fetches whatever the latest version of our config is; useful for regression
  # testing
  unstable = call (stablePkgs.latestGit { inherit url; });
}
