{-# LANGUAGE DataKinds #-}

module Database.Esqueleto.Postgis.Spatial
  ( st_contains
  , st_intersects
  , st_within
  , st_touches
  , st_crosses
  , st_disjoint
  , st_equals
  , st_covers
  , st_coveredby
  , st_overlaps
  , st_containsproperly
  , st_3dintersects
  , st_relate
  , st_orderingequals
  , st_dfullywithin
  , st_pointinsidecircle
  ) where

import Database.Esqueleto.Postgis.Geometry (Postgis, SpatialType(..))
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Data.Text (Text)

-- | Returns TRUE if geometry A contains geometry B.
--   https://postgis.net/docs/ST_Contains.html
st_contains ::
  -- | geom a
  SqlExpr (Value (Postgis 'Geometry a)) ->
  -- | geom b
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_contains a b = unsafeSqlFunction "ST_CONTAINS" (a, b)

-- | Returns true if two geometries intersect.
--   Geometries intersect if they have any point in common.
--   https://postgis.net/docs/ST_Intersects.html
st_intersects ::
  SqlExpr (Value (Postgis spatialType a)) -> -- ^ geomA or geogA
  SqlExpr (Value (Postgis spatialType a)) -> -- ^ geomB or geogB
  SqlExpr (Value Bool)
st_intersects a b = unsafeSqlFunction "ST_Intersects" (a, b)

-- | Returns TRUE if geometry A is within geometry B.
--   Tests if every point of A lies inside (interior or boundary of) B.
--   The inverse of 'st_contains': @st_within a b == st_contains b a@.
--   https://postgis.net/docs/ST_Within.html
st_within ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_within a b = unsafeSqlFunction "ST_Within" (a, b)

-- | Returns TRUE if geometry A touches geometry B.
--   They have at least one boundary point in common, but no interior points.
--   https://postgis.net/docs/ST_Touches.html
st_touches ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_touches a b = unsafeSqlFunction "ST_Touches" (a, b)

-- | Returns TRUE if geometry A crosses geometry B.
--   They have some but not all interior points in common.
--   https://postgis.net/docs/ST_Crosses.html
st_crosses ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_crosses a b = unsafeSqlFunction "ST_Crosses" (a, b)

-- | Returns TRUE if geometry A is disjoint from geometry B.
--   They do not share any space together, the inverse of 'st_intersects'.
--   https://postgis.net/docs/ST_Disjoint.html
st_disjoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_disjoint a b = unsafeSqlFunction "ST_Disjoint" (a, b)

-- | Returns TRUE if geometry A is spatially equal to geometry B.
--   The geometries represent the same region of space regardless of vertex order.
--   https://postgis.net/docs/ST_Equals.html
st_equals ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_equals a b = unsafeSqlFunction "ST_Equals" (a, b)

-- | Returns TRUE if geometry/geography A covers geometry/geography B.
--   No point in B is outside A. Similar to 'st_contains' but does not distinguish boundary and interior.
--   https://postgis.net/docs/ST_Covers.html
st_covers ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Bool)
st_covers a b = unsafeSqlFunction "ST_Covers" (a, b)

-- | Returns TRUE if geometry/geography A is covered by geometry/geography B.
--   No point in A is outside B. The inverse of 'st_covers'.
--   https://postgis.net/docs/ST_CoveredBy.html
st_coveredby ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Bool)
st_coveredby a b = unsafeSqlFunction "ST_CoveredBy" (a, b)

-- | Returns TRUE if geometry A overlaps geometry B.
--   They share some space but neither contains the other entirely.
--   https://postgis.net/docs/ST_Overlaps.html
st_overlaps ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_overlaps a b = unsafeSqlFunction "ST_Overlaps" (a, b)

-- | Returns TRUE if geometry A contains geometry B properly.
--   B must lie entirely inside the interior of A (not touching the boundary).
--   https://postgis.net/docs/ST_ContainsProperly.html
st_containsproperly ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_containsproperly a b = unsafeSqlFunction "ST_ContainsProperly" (a, b)

-- | Returns TRUE if two 3D geometries intersect.
--   https://postgis.net/docs/ST_3DIntersects.html
st_3dintersects ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_3dintersects a b = unsafeSqlFunction "ST_3DIntersects" (a, b)

-- | Returns the DE-9IM intersection matrix string for two geometries.
--   https://postgis.net/docs/ST_Relate.html
st_relate ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_relate a b = unsafeSqlFunction "ST_Relate" (a, b)

-- | Returns TRUE if two geometries are point-by-point equal in the same order.
--   https://postgis.net/docs/ST_OrderingEquals.html
st_orderingequals ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_orderingequals a b = unsafeSqlFunction "ST_OrderingEquals" (a, b)

-- | Returns TRUE if all of the geometries are within the specified distance of one another.
--   https://postgis.net/docs/ST_DFullyWithin.html
st_dfullywithin ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value Bool)
st_dfullywithin a b d = unsafeSqlFunction "ST_DFullyWithin" (a, b, d)

-- | Returns TRUE if the point geometry is inside the circle defined by center_x, center_y and radius.
--   https://postgis.net/docs/ST_PointInsideCircle.html
st_pointinsidecircle ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ center_x
  SqlExpr (Value Double) -> -- ^ center_y
  SqlExpr (Value Double) -> -- ^ radius
  SqlExpr (Value Bool)
st_pointinsidecircle a cx cy r = unsafeSqlFunction "ST_PointInsideCircle" (a, cx, cy, r)
