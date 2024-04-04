[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/esqueleto-postgis/Test)](https://github.com/jappeace/esqueleto-postgis/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/esqueleto-postgis.svg?label=Hackage)](https://hackage.haskell.org/package/esqueleto-postgis) 

> Show me the place where space is not.

Implement (partial) postgis functionality for esqueleto.
https://postgis.net/

uses wkt-geom to get a persistent instance,
then maps that to a custom datatype 'PostgisGeometry' which is valid
for roundtripping.

Then the esqueleto combinators are defined around this datatype.

# Tutorial

most linux distributions support this out of the box.
nixos needs some special care:
```nix
services.postgresql = {
   enable = true;
   package = (pkgs.postgresql_12.withPackages (p: [ p.postgis ]));
};
```
For mac you've to [install](https://postgis.net/documentation/getting_started/install_macos/) postgis.
```sh
brew install postgis
```

Make sure to enable to postgis extension on your database (it's activated per database):
```sql
CREATE EXTENSION postgis;
```

you can specify some posgis geometry,
use the point to nidicate dimensions, 
pointxy = 2 dimensions
pointxyz = 3, pointxyzm = 4.
The library forces you to work in the same dimensions.

You can specify a table with the custom datatype, which will have geometry as sql type:

```haskell
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistUpperCase|
  Unit sql=unit
    geom       (PostgisGeometry PointXY)
    deriving Eq Show
|]
```

then you can simply query on tat datatype:

```haskell
test = testCase ("it finds the one unit with st_contains") $ do
            result <- runDB $ do
              _ <- insert $
                Unit
                  { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)]
                  }

              selectOne $ do
                unit <- from $ table @Unit
                where_ $ unit ^. UnitGeom `st_contains` (val $ Point (PointXY 1 1))
                pure countRows

            -- expectation, the result should be 1
            unValue <$> result @?= (Just (1 :: Int)),
```

# Contributing
contributions are welcome.
There are still many bindings missing!

# Hacking

### Tools
Enter the nix shell.
```
nix develop
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
