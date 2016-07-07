with import <nixpkgs> {};

rec {
  isaplannerDocker = stdenv.mkDerivation rec {
    name = "isaplanner";
    src  = fetchFromGitHub {
      owner  = "TheoryMine";
      repo   = "IsaPlanner";
      rev    = "f3f4220";
      sha256 = "xkzxfdnqnx9p43r40vjwskklqn0mqxg6";
    };


    buildPhase = ''
      docker build -t theorymine/isaplanner:2015.0.2 .
    '';

    installPhase = ''
      mkdir -p "$out/bin"
      cp "${isaplannerRun}" "$out/bin/isaplanner-run"
    '';

    isaplannerRun = writeFile "isaplanner-run" ''
      docker run -i -t theorymine/isaplanner:2015.0.2 /bin/bash
    '';
  };
}
