{-# LANGUAGE DataKinds #-}

module Database.Esqueleto.Postgis.Accessor
  ( st_x
  , st_y
  , st_npoints
  , st_numgeometries
  , st_dimension
  , st_issimple
  , st_isclosed
  , st_isvalid
  , st_srid
  , st_boundary
  , st_coorddim
  , st_endpoint
  , st_exteriorring
  , st_geometryn
  , st_geometrytype
  , st_interiorringn
  , st_iscollection
  , st_isempty
  , st_ispolygonccw
  , st_ispolygoncw
  , st_isring
  , st_m
  , st_ndims
  , st_nrings
  , st_numinteriorrings
  , st_numpoints
  , st_pointn
  , st_startpoint
  , st_z
  ) where

import Database.Esqueleto.Postgis.Geometry (Postgis, SpatialType(..))
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Data.Text (Text)

-- | Returns the X coordinate of a point geometry.
--   Only works on points; other geometry types will error.
--   https://postgis.net/docs/ST_X.html
st_x ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_x a = unsafeSqlFunction "ST_X" a

-- | Returns the Y coordinate of a point geometry.
--   Only works on points; other geometry types will error.
--   https://postgis.net/docs/ST_Y.html
st_y ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_y a = unsafeSqlFunction "ST_Y" a

-- | Returns the number of points (vertices) in a geometry.
--   Works on any geometry type.
--   https://postgis.net/docs/ST_NPoints.html
st_npoints ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_npoints a = unsafeSqlFunction "ST_NPoints" a

-- | Returns the number of sub-geometries in a geometry collection or multi-type.
--   Returns 1 for single geometries.
--   https://postgis.net/docs/ST_NumGeometries.html
st_numgeometries ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_numgeometries a = unsafeSqlFunction "ST_NumGeometries" a

-- | Returns the topological dimension of a geometry.
--   0 for points, 1 for lines, 2 for polygons.
--   https://postgis.net/docs/ST_Dimension.html
st_dimension ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_dimension a = unsafeSqlFunction "ST_Dimension" a

-- | Returns TRUE if the geometry has no self-intersections.
--   Points and properly-formed polygons are always simple.
--   https://postgis.net/docs/ST_IsSimple.html
st_issimple ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_issimple a = unsafeSqlFunction "ST_IsSimple" a

-- | Returns TRUE if the linestring's start and end points are coincident.
--   For polyhedral surfaces, reports if the surface is areal (open) or volumetric (closed).
--   https://postgis.net/docs/ST_IsClosed.html
st_isclosed ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_isclosed a = unsafeSqlFunction "ST_IsClosed" a

-- | Returns TRUE if the geometry is well-formed and valid per the OGC rules.
--   Points and lines are always valid; polygons need correct ring structure.
--   https://postgis.net/docs/ST_IsValid.html
st_isvalid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_isvalid a = unsafeSqlFunction "ST_IsValid" a

-- | Returns the spatial reference identifier (SRID) of the geometry.
--   0 means no SRID is set.
--   https://postgis.net/docs/ST_SRID.html
st_srid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_srid a = unsafeSqlFunction "ST_SRID" a

-- | Returns the boundary of a geometry.
--   https://postgis.net/docs/ST_Boundary.html
st_boundary ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_boundary a = unsafeSqlFunction "ST_Boundary" a

-- | Returns the coordinate dimension of a geometry (2, 3, or 4).
--   https://postgis.net/docs/ST_CoordDim.html
st_coorddim ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_coorddim a = unsafeSqlFunction "ST_CoordDim" a

-- | Returns the last point of a linestring.
--   https://postgis.net/docs/ST_EndPoint.html
st_endpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_endpoint a = unsafeSqlFunction "ST_EndPoint" a

-- | Returns the exterior ring of a polygon geometry.
--   https://postgis.net/docs/ST_ExteriorRing.html
st_exteriorring ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_exteriorring a = unsafeSqlFunction "ST_ExteriorRing" a

-- | Returns the Nth sub-geometry of a geometry collection (1-indexed).
--   https://postgis.net/docs/ST_GeometryN.html
st_geometryn ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_geometryn a n = unsafeSqlFunction "ST_GeometryN" (a, n)

-- | Returns the geometry type as a string (e.g. "ST_Polygon").
--   https://postgis.net/docs/ST_GeometryType.html
st_geometrytype ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_geometrytype a = unsafeSqlFunction "ST_GeometryType" a

-- | Returns the Nth interior ring of a polygon (1-indexed).
--   https://postgis.net/docs/ST_InteriorRingN.html
st_interiorringn ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_interiorringn a n = unsafeSqlFunction "ST_InteriorRingN" (a, n)

-- | Returns TRUE if the geometry is a geometry collection type.
--   https://postgis.net/docs/ST_IsCollection.html
st_iscollection ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_iscollection a = unsafeSqlFunction "ST_IsCollection" a

-- | Returns TRUE if the geometry is an empty geometry.
--   https://postgis.net/docs/ST_IsEmpty.html
st_isempty ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_isempty a = unsafeSqlFunction "ST_IsEmpty" a

-- | Returns TRUE if all polygon rings are oriented counter-clockwise.
--   https://postgis.net/docs/ST_IsPolygonCCW.html
st_ispolygonccw ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_ispolygonccw a = unsafeSqlFunction "ST_IsPolygonCCW" a

-- | Returns TRUE if all polygon rings are oriented clockwise.
--   https://postgis.net/docs/ST_IsPolygonCW.html
st_ispolygoncw ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_ispolygoncw a = unsafeSqlFunction "ST_IsPolygonCW" a

-- | Returns TRUE if the linestring is closed and simple (a ring).
--   https://postgis.net/docs/ST_IsRing.html
st_isring ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_isring a = unsafeSqlFunction "ST_IsRing" a

-- | Returns the M coordinate of a point.
--   https://postgis.net/docs/ST_M.html
st_m ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_m a = unsafeSqlFunction "ST_M" a

-- | Returns the number of dimensions of a geometry's coordinates (2, 3, or 4).
--   https://postgis.net/docs/ST_NDims.html
st_ndims ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_ndims a = unsafeSqlFunction "ST_NDims" a

-- | Returns the number of rings in a polygon (exterior + interior).
--   https://postgis.net/docs/ST_NRings.html
st_nrings ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_nrings a = unsafeSqlFunction "ST_NRings" a

-- | Returns the number of interior rings of a polygon.
--   https://postgis.net/docs/ST_NumInteriorRings.html
st_numinteriorrings ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_numinteriorrings a = unsafeSqlFunction "ST_NumInteriorRings" a

-- | Returns the number of points in a linestring.
--   https://postgis.net/docs/ST_NumPoints.html
st_numpoints ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int)
st_numpoints a = unsafeSqlFunction "ST_NumPoints" a

-- | Returns the Nth point in a linestring (1-indexed).
--   https://postgis.net/docs/ST_PointN.html
st_pointn ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_pointn a n = unsafeSqlFunction "ST_PointN" (a, n)

-- | Returns the first point of a linestring.
--   https://postgis.net/docs/ST_StartPoint.html
st_startpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_startpoint a = unsafeSqlFunction "ST_StartPoint" a

-- | Returns the Z coordinate of a point.
--   https://postgis.net/docs/ST_Z.html
st_z ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_z a = unsafeSqlFunction "ST_Z" a
