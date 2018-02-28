with (import ./pkgs.nix).stable;
runCommand "shell-env"
  {
    benchmarker = import ./benchmarks;
    buildInputs = [ asv-nix ];
  }
  "exit 1"
