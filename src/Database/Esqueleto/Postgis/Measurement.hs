{-# LANGUAGE DataKinds #-}

module Database.Esqueleto.Postgis.Measurement
  ( st_distance
  , st_dwithin
  , st_area
  , st_perimeter
  , st_length
  , st_azimuth
  , st_maxdistance
  , st_angle
  , st_closestpoint
  , st_3dclosestpoint
  , st_3ddistance
  , st_distancesphere
  , st_frechetdistance
  , st_hausdorffdistance
  , st_length2d
  , st_3dlength
  , st_longestline
  , st_3dlongestline
  , st_3dmaxdistance
  , st_minimumclearance
  , st_shortestline
  , st_3dshortestline
  ) where

import Database.Esqueleto.Postgis.Geometry (Postgis, SpatialType(..))
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)

-- | calculate the distance between two points
--   https://postgis.net/docs/ST_Distance.html
st_distance ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Double)
st_distance a b =
  unsafeSqlFunction "ST_distance" (a, b)

-- | Returns true if the geometries are within a given distance
--   https://postgis.net/docs/ST_DWithin.html
st_dwithin ::
  -- | geometry g1
  SqlExpr (Value (Postgis spatialType a)) ->
  -- | geometry g2
  SqlExpr (Value (Postgis spatialType a)) ->
  -- | distance of srid
  SqlExpr (Value Double) ->
  SqlExpr (Value Bool)
st_dwithin a b c = unsafeSqlFunction "ST_DWithin" (a, b, c)

-- | Returns the area of a geometry or geography.
--   For geometry, area is in SRID units squared. For geography, area is in square meters.
--   https://postgis.net/docs/ST_Area.html
st_area ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Double)
st_area a = unsafeSqlFunction "ST_Area" a

-- | Returns the perimeter of a geometry or geography.
--   For geometry, in SRID units. For geography, in meters.
--   https://postgis.net/docs/ST_Perimeter.html
st_perimeter ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Double)
st_perimeter a = unsafeSqlFunction "ST_Perimeter" a

-- | Returns the 2D length of a linear geometry or geography.
--   For geometry, in SRID units. For geography, in meters.
--   https://postgis.net/docs/ST_Length.html
st_length ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Double)
st_length a = unsafeSqlFunction "ST_Length" a

-- | Returns the azimuth in radians of the segment from point A to point B.
--   The angle is measured clockwise from north (positive Y axis).
--   https://postgis.net/docs/ST_Azimuth.html
st_azimuth ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_azimuth a b = unsafeSqlFunction "ST_Azimuth" (a, b)

-- | Returns the maximum distance between two geometries.
--   The distance of the furthest pair of points.
--   https://postgis.net/docs/ST_MaxDistance.html
st_maxdistance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_maxdistance a b = unsafeSqlFunction "ST_MaxDistance" (a, b)

-- | Returns the angle between two vectors defined by two 2-point linestrings.
--   This is the 2-argument form of ST_Angle which expects linestring inputs.
--   Passing point geometries will result in a NULL return value — use the
--   3 or 4 point overloads in PostGIS directly if you need point-based angles.
--   https://postgis.net/docs/ST_Angle.html
st_angle ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_angle a b = unsafeSqlFunction "ST_Angle" (a, b)

-- | Returns the 2D point on geom1 closest to geom2.
--   https://postgis.net/docs/ST_ClosestPoint.html
st_closestpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_closestpoint a b = unsafeSqlFunction "ST_ClosestPoint" (a, b)

-- | Returns the 3D point on geom1 closest to geom2.
--   https://postgis.net/docs/ST_3DClosestPoint.html
st_3dclosestpoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_3dclosestpoint a b = unsafeSqlFunction "ST_3DClosestPoint" (a, b)

-- | Returns the 3D cartesian minimum distance between two geometries.
--   https://postgis.net/docs/ST_3DDistance.html
st_3ddistance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_3ddistance a b = unsafeSqlFunction "ST_3DDistance" (a, b)

-- | Returns the minimum distance in meters between two lon/lat geometries using a spherical model.
--   https://postgis.net/docs/ST_DistanceSphere.html
st_distancesphere ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_distancesphere a b = unsafeSqlFunction "ST_DistanceSphere" (a, b)

-- | Returns the Frechet distance between two geometries.
--   https://postgis.net/docs/ST_FrechetDistance.html
st_frechetdistance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_frechetdistance a b = unsafeSqlFunction "ST_FrechetDistance" (a, b)

-- | Returns the Hausdorff distance between two geometries.
--   https://postgis.net/docs/ST_HausdorffDistance.html
st_hausdorffdistance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_hausdorffdistance a b = unsafeSqlFunction "ST_HausdorffDistance" (a, b)

-- | Returns the 2D length of a linear geometry.
--   https://postgis.net/docs/ST_Length2D.html
st_length2d ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_length2d a = unsafeSqlFunction "ST_Length2D" a

-- | Returns the 3D length of a linear geometry.
--   https://postgis.net/docs/ST_3DLength.html
st_3dlength ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_3dlength a = unsafeSqlFunction "ST_3DLength" a

-- | Returns the 2D longest line between two geometries.
--   https://postgis.net/docs/ST_LongestLine.html
st_longestline ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_longestline a b = unsafeSqlFunction "ST_LongestLine" (a, b)

-- | Returns the 3D longest line between two geometries.
--   https://postgis.net/docs/ST_3DLongestLine.html
st_3dlongestline ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_3dlongestline a b = unsafeSqlFunction "ST_3DLongestLine" (a, b)

-- | Returns the 3D maximum distance between two geometries.
--   https://postgis.net/docs/ST_3DMaxDistance.html
st_3dmaxdistance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_3dmaxdistance a b = unsafeSqlFunction "ST_3DMaxDistance" (a, b)

-- | Returns the minimum clearance of a geometry, the shortest distance between two distinct vertices.
--   https://postgis.net/docs/ST_MinimumClearance.html
st_minimumclearance ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double)
st_minimumclearance a = unsafeSqlFunction "ST_MinimumClearance" a

-- | Returns the 2D shortest line between two geometries.
--   https://postgis.net/docs/ST_ShortestLine.html
st_shortestline ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_shortestline a b = unsafeSqlFunction "ST_ShortestLine" (a, b)

-- | Returns the 3D shortest line between two geometries.
--   https://postgis.net/docs/ST_3DShortestLine.html
st_3dshortestline ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_3dshortestline a b = unsafeSqlFunction "ST_3DShortestLine" (a, b)
