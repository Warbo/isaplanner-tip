with import <nixpkgs> {};

rec {
  isaplannerDocker = stdenv.mkDerivation rec {
    name = "isaplanner";
    src  = fetchFromGitHub {
      owner  = "TheoryMine";
      repo   = "IsaPlanner";
      rev    = "f3f4220";
      sha256 = "0kw1qslzv1a7fq25rhashcmnsnp16vfy92y3n0mccqs5ll2wf4f4";
    };


    buildPhase = ''
      docker build -t theorymine/isaplanner:2015.0.2 .
    '';

    installPhase = ''
      mkdir -p "$out/bin"
      cp "${isaplannerRun}" "$out/bin/isaplanner-run"
    '';

    isaplannerRun = writeScript "isaplanner-run" ''
      docker run -i -t theorymine/isaplanner:2015.0.2 /bin/bash
    '';
  };
}
