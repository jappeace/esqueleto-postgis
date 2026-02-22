{
  hpkgs ? import ./hpkgs.nix { },
  pkgs ? import ./pkgs.nix { },
}:
let
  package = import ../default.nix { inherit hpkgs; } ;
in
{
  build = package;
  integrated-checks = pkgs.testers.nixosTest {
        name = "esqueleto-test";

        testScript = ''
          server.start()
          server.wait_for_unit("postgresql.service")
          # Wait explicitly for the PostGIS extension to be ready
          server.wait_until_succeeds(
            "sudo -u postgres psql -d test -c 'SELECT PostGIS_Version()'"
          )
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
}
