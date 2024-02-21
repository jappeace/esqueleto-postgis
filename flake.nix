{
  description = "esqueleto postgis";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          esqueleto-postgis = hnew.callCabal2nix "esqueleto-postgis" ./. { };
          wkt-geom = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.markUnbroken hold.wkt-geom);
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.esqueleto-postgis-project;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps.esqueleto-postgis ];
        withHoogle = false;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
