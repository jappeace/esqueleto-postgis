{
  pkgs ? import ./pkgs.nix { },
}:
# you can pin a specific ghc version with
# pkgs.haskell.packages.ghc984 for example.
# this allows you to create multiple compiler targets via nix.
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    esqueleto-postgis =
      pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "esqueleto-postgis" ../. { })
        {
          postBuild = ''
            echo "entering the phase"
            mkdir -p $out/bin/test
            cp ./dist/build/unit/unit $out/bin/test/unit
          '';

          checkPhase = ''
            echo "ran by flake :)"
          '';
        };
    geojson = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.markUnbroken hold.geojson);
  };
}
