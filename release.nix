with import ./pkgs.nix;
with stable.lib;
with rec {
  # Turn anything that's not a derivation or set into null, so Hydra skips it
  onlyDrvs = x: if isAttrs x
                   then if isDerivation x
                           then x
                           else mapAttrs (_: onlyDrvs) x
                   else null;

  # These are large sets (e.g. nixpkgs) which aren't part of this project
  unwantedPaths = [
    [ "defs" "haskell-te"           "haskell-te"      ]
    [ "defs" "tebenchmark-isabelle" "haskellPackages" ]
    [ "defs" "tebenchmark-isabelle" "nixpkgs1703"     ]
    [ "defs" "tebenchmark-isabelle" "te-benchmark"    ]
  ];

  # Recurse through attribute sets, accumulating a path, and checking if it's
  # one of the unwantedPaths above. Replace with null if so.
  stripUnwanted = path: n: v:
    if elem (path ++ [n]) unwantedPaths
       then null
       else if isAttrs v
               then if isDerivation v
                       then v
                       else mapAttrs (stripUnwanted (path ++ [n])) v
               else v;

  # Initialise stripUnwanted with an empty path, and apply to stable/unstable
  all = mapAttrs (_: pkgs: mapAttrs (stripUnwanted [])
                                    (import ./. { inherit pkgs; }))
                 { inherit stable unstable; };
};
onlyDrvs all
