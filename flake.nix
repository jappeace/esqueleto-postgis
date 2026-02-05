{
  description = "esqueleto postgis";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs,  }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          esqueleto-postgis = pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "esqueleto-postgis" ./. { }) {
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
      };
      package = hpkgs.esqueleto-postgis;
    in
    {
      defaultPackage.x86_64-linux =  package;
      inherit pkgs;

      checks.x86_64-linux.tests =  pkgs.nixosTest {
        name = "esqueleto-test";

        testScript = ''
          server.start()
          server.wait_for_unit("postgresql.service")
          print(
              server.succeed(
                  "${package}/bin/test/unit"
              )
          )
        '';
        nodes.server = {

          virtualisation.memorySize = 2048;
          virtualisation.diskSize = 1024;
          services.postgresql = {
            package = (pkgs.postgresql.withPackages (p: [ p.postgis ]));
            enable = true;
            initialScript = pkgs.writeText "psql-init" ''
              CREATE USER test WITH SUPERUSER PASSWORD 'test';
              CREATE DATABASE test WITH OWNER test;
              \c test;
              CREATE EXTENSION postgis;
            '';
          };
        };
      };
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
