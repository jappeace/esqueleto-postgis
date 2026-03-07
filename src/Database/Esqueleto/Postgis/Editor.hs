{-# LANGUAGE DataKinds #-}

module Database.Esqueleto.Postgis.Editor
  ( st_addpoint
  , st_collectionextract
  , st_flipcoordinates
  , st_force2d
  , st_force3d
  , st_force4d
  , st_forcecollection
  , st_forcepolygonccw
  , st_forcepolygoncw
  , st_multi
  , st_normalize
  , st_reverse
  , st_segmentize
  , st_setpoint
  , st_snaptogrid
  , st_snap
  ) where

import Database.Esqueleto.Postgis.Geometry (Postgis, SpatialType(..))
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)

-- | Adds a point to a linestring at a given position (0-indexed).
--   https://postgis.net/docs/ST_AddPoint.html
st_addpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ linestring
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ point
  SqlExpr (Value Int) ->                   -- ^ position
  SqlExpr (Value (Postgis 'Geometry a))
st_addpoint a b n = unsafeSqlFunction "ST_AddPoint" (a, b, n)

-- | Extracts sub-geometries of a specified type from a collection.
--   Type: 1=POINT, 2=LINESTRING, 3=POLYGON.
--   https://postgis.net/docs/ST_CollectionExtract.html
st_collectionextract ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_collectionextract a t = unsafeSqlFunction "ST_CollectionExtract" (a, t)

-- | Swaps X and Y coordinates.
--   https://postgis.net/docs/ST_FlipCoordinates.html
st_flipcoordinates ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_flipcoordinates a = unsafeSqlFunction "ST_FlipCoordinates" a

-- | Forces the geometry into 2D mode.
--   https://postgis.net/docs/ST_Force2D.html
st_force2d ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_force2d a = unsafeSqlFunction "ST_Force2D" a

-- | Forces the geometry into 3D (XYZ) mode.
--   https://postgis.net/docs/ST_Force3D.html
st_force3d ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_force3d a = unsafeSqlFunction "ST_Force3D" a

-- | Forces the geometry into 4D (XYZM) mode.
--   https://postgis.net/docs/ST_Force4D.html
st_force4d ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_force4d a = unsafeSqlFunction "ST_Force4D" a

-- | Converts the geometry into a geometry collection.
--   https://postgis.net/docs/ST_ForceCollection.html
st_forcecollection ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_forcecollection a = unsafeSqlFunction "ST_ForceCollection" a

-- | Forces polygon rings to counter-clockwise orientation.
--   https://postgis.net/docs/ST_ForcePolygonCCW.html
st_forcepolygonccw ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_forcepolygonccw a = unsafeSqlFunction "ST_ForcePolygonCCW" a

-- | Forces polygon rings to clockwise orientation.
--   https://postgis.net/docs/ST_ForcePolygonCW.html
st_forcepolygoncw ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_forcepolygoncw a = unsafeSqlFunction "ST_ForcePolygonCW" a

-- | Returns the geometry as a multi-type (e.g. POINT -> MULTIPOINT).
--   https://postgis.net/docs/ST_Multi.html
st_multi ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_multi a = unsafeSqlFunction "ST_Multi" a

-- | Returns the geometry in its canonical (normalized) form.
--   https://postgis.net/docs/ST_Normalize.html
st_normalize ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_normalize a = unsafeSqlFunction "ST_Normalize" a

-- | Returns the geometry with vertex order reversed.
--   https://postgis.net/docs/ST_Reverse.html
st_reverse ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_reverse a = unsafeSqlFunction "ST_Reverse" a

-- | Segmentizes a geometry so no segment is longer than the given distance.
--   https://postgis.net/docs/ST_Segmentize.html
st_segmentize ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_segmentize a d = unsafeSqlFunction "ST_Segmentize" (a, d)

-- | Replaces a point of a linestring at a given 0-based index.
--   https://postgis.net/docs/ST_SetPoint.html
st_setpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ linestring
  SqlExpr (Value Int) ->                   -- ^ 0-based index
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ new point
  SqlExpr (Value (Postgis 'Geometry a))
st_setpoint a n p = unsafeSqlFunction "ST_SetPoint" (a, n, p)

-- | Snaps all points of a geometry to a regular grid of the given size.
--   https://postgis.net/docs/ST_SnapToGrid.html
st_snaptogrid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_snaptogrid a d = unsafeSqlFunction "ST_SnapToGrid" (a, d)

-- | Snaps vertices of one geometry to vertices and edges of another within a tolerance.
--   https://postgis.net/docs/ST_Snap.html
st_snap ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_snap a b d = unsafeSqlFunction "ST_Snap" (a, b, d)
