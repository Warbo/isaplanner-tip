{ fetchgit }:

with builtins;
rec {
  get-haskell-te = rev: rec {
    haskell-te     = import "${haskell-te-src}/nix-support" {};
    haskell-te-src = fetchgit {
      inherit rev;
      url    = http://chriswarbo.net/git/haskell-te.git;
      sha256 = getAttr rev haskell-te-hashes;
    };
  };

  # Known versions of haskell-te, e.g. those used for particular benchmark runs
  haskell-te-hashes = {
    "7a4cc07a" = "04s9b60c6l2jji85ksmgab2gbxyddjxx4ps39rynjxr3fv1w550m";
    "ce9c9478" = "0bmdjz13ya3c3lfc2paf2bcc1wmhyld7qqx492sdvjknjnf4p9cb";
    "334d529"  = "109g8hkpggjjlw7ksd7l157jknp4wkg9lbjlyiqqvqzah2kl65jf";
  };

  inherit (get-haskell-te "7a4cc07a") haskell-te haskell-te-src;
}
