{ fetchgit }:

rec {
  get-haskell-te = { rev , sha256 }: fetchgit {
    inherit rev sha256;
    url    = http://chriswarbo.net/git/haskell-te.git;
  };

  haskell-te-src = get-haskell-te {
    rev    = "7a4cc07a";
    sha256 = "04s9b60c6l2jji85ksmgab2gbxyddjxx4ps39rynjxr3fv1w550m";
  };

  haskell-te = import "${haskell-te-src}/nix-support" {};
}
