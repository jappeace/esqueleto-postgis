{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Esqueleto.Postgis.Processing
  ( st_union
  , st_unions
  , st_centroid
  , st_buffer
  , st_convexhull
  , st_envelope
  , st_pointonsurface
  , st_intersection
  , st_difference
  , st_symdifference
  , st_unaryunion
  , st_split
  , st_node
  , st_buildarea
  , st_chaikinsmoothing
  , st_concavehull
  , st_delaunaytriangles
  , st_generatepoints
  , st_geometricmedian
  , st_linemerge
  , st_minimumboundingcircle
  , st_offsetcurve
  , st_reduceprecision
  , st_sharedpaths
  , st_simplify
  , st_simplifypreservetopology
  , st_voronoilines
  , st_voronoipolygons
  ) where

import Data.Proxy
import Database.Esqueleto.Postgis.Geometry (Postgis, SpatialType(..), HasPgType(..))
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction, unsafeSqlCastAs)

-- | allows union of geometries, eg group a bunch together,
--   https://postgis.net/docs/ST_Union.html
--   for example:
--
-- @
--  mCombined <- selectOne $ do
--    grid <- from $ table @Grid
--    pure $ st_union $ grid ^. GridGeom
--
--
--  select $  do
--    unit <- from $ table @Unit
--    forM_ mCombined $ \combined ->
--      where_ $ (unit ^. UnitGeom) `st_intersects` (val $ unValue combined)
--    pure unit
-- @
st_union ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_union a = unsafeSqlFunction "ST_union" a

st_unions ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_unions a b =
  -- casts to prevent
  -- function st_union(unknown, unknown) is not unique", sqlErrorDetail = "", sqlErrorHint = "Could not choose a best candidate function. You might need to add explicit type casts.
  unsafeSqlFunction "ST_union" ((unsafeSqlCastAs casted a), (unsafeSqlCastAs casted b))
  where
    casted = (pgType $ Proxy @'Geometry)

-- | Returns the geometric center (centroid) of a geometry.
--   For polygons this is the center of mass of the surface.
--   https://postgis.net/docs/ST_Centroid.html
st_centroid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_centroid a = unsafeSqlFunction "ST_Centroid" a

-- | Returns a geometry that is the buffer of the input geometry at a given distance.
--   The buffer is a polygon expanded (or shrunk if negative) from the input by the distance.
--   https://postgis.net/docs/ST_Buffer.html
st_buffer ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_buffer a d = unsafeSqlFunction "ST_Buffer" (a, d)

-- | Returns the convex hull of a geometry.
--   The smallest convex polygon that contains the input geometry.
--   https://postgis.net/docs/ST_ConvexHull.html
st_convexhull ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_convexhull a = unsafeSqlFunction "ST_ConvexHull" a

-- | Returns the bounding box of a geometry as a geometry (polygon or point).
--   A minimal axis-aligned rectangle that fully contains the input.
--   https://postgis.net/docs/ST_Envelope.html
st_envelope ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_envelope a = unsafeSqlFunction "ST_Envelope" a

-- | Returns a point guaranteed to lie on the surface of the geometry.
--   Unlike 'st_centroid', the result is always on or inside the geometry.
--   https://postgis.net/docs/ST_PointOnSurface.html
st_pointonsurface ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_pointonsurface a = unsafeSqlFunction "ST_PointOnSurface" a

-- | Returns the shared portion of two geometries (their intersection).
--   The result geometry contains only the area common to both inputs.
--   https://postgis.net/docs/ST_Intersection.html
st_intersection ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_intersection a b = unsafeSqlFunction "ST_Intersection" (a, b)

-- | Returns the part of geometry A that does not intersect geometry B.
--   Computes the geometric difference: A minus the shared area of A and B.
--   https://postgis.net/docs/ST_Difference.html
st_difference ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_difference a b = unsafeSqlFunction "ST_Difference" (a, b)

-- | Returns the portions of A and B that do not intersect (symmetric difference).
--   https://postgis.net/docs/ST_SymDifference.html
st_symdifference ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_symdifference a b = unsafeSqlFunction "ST_SymDifference" (a, b)

-- | Computes the union of a single geometry (dissolves internal boundaries).
--   https://postgis.net/docs/ST_UnaryUnion.html
st_unaryunion ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_unaryunion a = unsafeSqlFunction "ST_UnaryUnion" a

-- | Splits a geometry by another geometry, returning a collection of geometries.
--   https://postgis.net/docs/ST_Split.html
st_split ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_split a b = unsafeSqlFunction "ST_Split" (a, b)

-- | Nodes a set of linestrings, adding intersection points.
--   https://postgis.net/docs/ST_Node.html
st_node ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_node a = unsafeSqlFunction "ST_Node" a

-- | Creates an areal geometry formed by the constituent linework of the input geometry.
--   https://postgis.net/docs/ST_BuildArea.html
st_buildarea ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_buildarea a = unsafeSqlFunction "ST_BuildArea" a

-- | Returns a smoothed version of the geometry using Chaikin's algorithm.
--   https://postgis.net/docs/ST_ChaikinSmoothing.html
st_chaikinsmoothing ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_chaikinsmoothing a = unsafeSqlFunction "ST_ChaikinSmoothing" a

-- | Returns a concave hull of a geometry with a target percent of area.
--   https://postgis.net/docs/ST_ConcaveHull.html
st_concavehull ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ target_percent
  SqlExpr (Value (Postgis 'Geometry a))
st_concavehull a d = unsafeSqlFunction "ST_ConcaveHull" (a, d)

-- | Returns the Delaunay triangulation of a geometry's vertices.
--   https://postgis.net/docs/ST_DelaunayTriangles.html
st_delaunaytriangles ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_delaunaytriangles a = unsafeSqlFunction "ST_DelaunayTriangles" a

-- | Generates pseudo-random points within a polygon.
--   https://postgis.net/docs/ST_GeneratePoints.html
st_generatepoints ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) -> -- ^ npoints
  SqlExpr (Value (Postgis 'Geometry a))
st_generatepoints a n = unsafeSqlFunction "ST_GeneratePoints" (a, n)

-- | Returns the geometric median of a multipoint.
--   https://postgis.net/docs/ST_GeometricMedian.html
st_geometricmedian ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_geometricmedian a = unsafeSqlFunction "ST_GeometricMedian" a

-- | Merges a collection of linestrings into a single (or fewer) linestrings.
--   https://postgis.net/docs/ST_LineMerge.html
st_linemerge ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_linemerge a = unsafeSqlFunction "ST_LineMerge" a

-- | Returns the minimum bounding circle enclosing a geometry.
--   https://postgis.net/docs/ST_MinimumBoundingCircle.html
st_minimumboundingcircle ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_minimumboundingcircle a = unsafeSqlFunction "ST_MinimumBoundingCircle" a

-- | Returns an offset line at a given distance from the input line.
--   https://postgis.net/docs/ST_OffsetCurve.html
st_offsetcurve ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_offsetcurve a d = unsafeSqlFunction "ST_OffsetCurve" (a, d)

-- | Reduces the precision of coordinates in a geometry to a given grid size.
--   https://postgis.net/docs/ST_ReducePrecision.html
st_reduceprecision ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_reduceprecision a d = unsafeSqlFunction "ST_ReducePrecision" (a, d)

-- | Returns a collection of shared paths between two linestring/multilinestring geometries.
--   https://postgis.net/docs/ST_SharedPaths.html
st_sharedpaths ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_sharedpaths a b = unsafeSqlFunction "ST_SharedPaths" (a, b)

-- | Returns a simplified version of a geometry using the Douglas-Peucker algorithm.
--   https://postgis.net/docs/ST_Simplify.html
st_simplify ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_simplify a d = unsafeSqlFunction "ST_Simplify" (a, d)

-- | Returns a simplified version of a geometry that preserves topology.
--   https://postgis.net/docs/ST_SimplifyPreserveTopology.html
st_simplifypreservetopology ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_simplifypreservetopology a d = unsafeSqlFunction "ST_SimplifyPreserveTopology" (a, d)

-- | Returns the Voronoi diagram edges for a set of points.
--   https://postgis.net/docs/ST_VoronoiLines.html
st_voronoilines ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_voronoilines a = unsafeSqlFunction "ST_VoronoiLines" a

-- | Returns the Voronoi diagram polygons for a set of points.
--   https://postgis.net/docs/ST_VoronoiPolygons.html
st_voronoipolygons ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_voronoipolygons a = unsafeSqlFunction "ST_VoronoiPolygons" a
