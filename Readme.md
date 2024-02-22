[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/jappeace/haskell-template-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/esqueleto-postgis.svg?label=Hackage)](https://hackage.haskell.org/package/esqueleto-postgis) 

> Show me the place where space is not.

Implement postgis functionality for esqueleto.
https://postgis.net/

uses wkt-geom to get a persistent instance,
then maps that to a custom datatype 'PostgisGeometry' which is valid
for roundtripping.

Then the esqueleto combinators are defined around this datatype.

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
