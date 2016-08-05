with builtins;

listToAttrs
  (map (system: {
         name  = system;
         value = import ./. {
                   pkgs = import <nixpkgs> { inherit system; };
                 };
       })
       [ "i686-linux" "x86_64-linux" ])
