with import ../pkgs.nix;
with import ../. { pkgs = stable; };
with rec {
  py = stable.python.withPackages (p: [ p.subprocess32 ]);
};
stable.mkBin {
  name  = "python";
  paths = [ py ];
  file  = "${py}/bin/python";
}
