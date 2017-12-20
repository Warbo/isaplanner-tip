with (import ./pkgs.nix).stable;
runCommand "shell-env"
  {
    buildInputs = [ asv-nix ];
  }
  "exit 1"
