# Change log for esqueleto-postgis project

## Version 2.2.0 
* add st_dwithin to find stuf within a range

## Version 2.1.0 
* add st_unions

## Version 2.0.1 
* drop haskell works hedghog dependency

## Version 2.0.0 
* Hide irrelevant modules
* delete hex module, use base16 hex approach instead.
* Much better docs that explain what's going on.

## Version 1.2.0 
+ re-export point, less annoying to use.
+ adopt wkt-geom package and put it in as a submodule,
  original library doesn't appear to be maintained.
  this way we're a step closer to stackage.
  + got rid of the internal convention, 
    it goes against pvp. If we want some specialized
    usage package I should split off a core package instead.
    For now I don't care I don't think I'm not changing those 
    functions anyway.
  + also ported over the test suite
+ swap out bytestring-base16 for base16, which appears maintained.


## Version 1.1.0 
+ Add st_union
+ add getPoitns to escape the postgis geometry more easily.
+ bump bounds


## Version 1.0.1 
+ fix insane bounds by cabal genbounds. 
  I think this was caused due to running it from the flake which
  takes a bunch of outdated packages from stackage.

## Version 1.0.0 
+ add st_contains
+ add st_intersects
+ add st_point
+ add custom datatype to map to persistent.
+ bunch of roundtrip tests and sanity tests for added functions

## Version 0.0.0 

import [template](https://github.com/jappeace/template).

