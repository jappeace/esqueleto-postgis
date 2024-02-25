# Change log for esqueleto-postgis project

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

