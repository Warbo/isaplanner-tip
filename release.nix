with import ./pkgs.nix;
with stable.lib;
with rec {
  onlyDrvs = x:
    if isAttrs x
       then if isDerivation x
               then x
               else with { result = filterAttrs (_: y: y != null)
                                                (mapAttrs (_: onlyDrvs) x); };
                    if result == {} then null else result
       else null;

  unwantedPaths = [
    [ "defs" "haskell-te"           "haskell-te"      ]
    [ "defs" "tebenchmark-isabelle" "haskellPackages" ]
    [ "defs" "tebenchmark-isabelle" "nixpkgs1703"     ]
    [ "defs" "tebenchmark-isabelle" "te-benchmark"    ]
  ];

  stripUnwanted = mapAttrsRecursive (path: _: v: if elem path unwantedPaths
                                                    then null
                                                    else v);

  all = mapAttrs (_: pkgs: stripUnwanted (import ./. { inherit pkgs; }))
                 { inherit stable unstable; };
};
onlyDrvs all
