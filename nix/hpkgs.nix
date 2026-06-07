{
  pkgs ? import ./pkgs.nix { },
}:
# you can pin a specific ghc version with
# pkgs.haskell.packages.ghc984 for example.
# this allows you to create multiple compiler targets via nix.
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    esqueleto-postgis =
      let
        nom = hnew.callCabal2nix "esqueleto-postgis" ../. { };
      in
      nom.overrideAttrs (oldAttrs: {
        outputs = (oldAttrs.outputs or [ "out" ]) ++ [ "test" ];
        postBuild = (oldAttrs.postBuild or "") + ''
          mkdir -p $test
          cp ./dist/build/unit/unit $test/unit
        '';
        checkPhase = "";
      });
    geojson = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.markUnbroken hold.geojson);
  };
}
