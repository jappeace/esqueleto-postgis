{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell bindings for postgres postgis
--   for a good explenation see <https://postgis.net/>
--
--   Make sure to use the correct 'SpatialType'.
--   Earth spanning applications should use Geography,
--   local applications should use 'Geometry' because it's more convenient.
--   You don't have to use Geography if you're only interested in
--   topological functions such as 'st_intersects' and 'st_union',
--   these are indifferent to space distortions,
--   see <https://www.gdmc.nl/publications/2013/3D_Geo-DBMS_Topological_Operators.pdf>
--   the related work section.
--
--   if you can't use a function for example when you're using 'Geography'.
--   there is the option to 'st_transform_geography'.
module Database.Esqueleto.Postgis
  (
    Postgis(..),
    SpatialType(..),
    getPoints,

    -- * Spatial relationship functions (Bool)
    st_contains,
    st_intersects,
    st_within,
    st_touches,
    st_crosses,
    st_disjoint,
    st_equals,
    st_covers,
    st_coveredby,
    st_overlaps,
    st_containsproperly,

    -- * Measurement functions (Double)
    st_distance,
    st_dwithin,
    st_area,
    st_perimeter,
    st_length,
    st_azimuth,
    st_maxdistance,

    -- * Geometry accessors
    st_x,
    st_y,
    st_npoints,
    st_numgeometries,
    st_dimension,
    st_issimple,
    st_isclosed,
    st_isvalid,
    st_srid,

    -- * Geometry constructors
    st_collect,
    st_makeenvelope,
    st_makeline,
    st_makepolygon_line,

    -- * Geometry accessors (additional)
    st_boundary,
    st_coorddim,
    st_endpoint,
    st_exteriorring,
    st_geometryn,
    st_geometrytype,
    st_interiorringn,
    st_iscollection,
    st_isempty,
    st_ispolygonccw,
    st_ispolygoncw,
    st_isring,
    st_m,
    st_ndims,
    st_nrings,
    st_numinteriorrings,
    st_numpoints,
    st_pointn,
    st_startpoint,
    st_z,

    -- * Geometry editors
    st_addpoint,
    st_collectionextract,
    st_flipcoordinates,
    st_force2d,
    st_force3d,
    st_force4d,
    st_forcecollection,
    st_forcepolygonccw,
    st_forcepolygoncw,
    st_multi,
    st_normalize,
    st_reverse,
    st_segmentize,
    st_setpoint,
    st_snaptogrid,
    st_snap,

    -- * Geometry validation
    st_makevalid,
    st_isvalidreason,

    -- * SRS functions
    st_setsrid,

    -- * Geometry output
    st_astext,
    st_asgeojson,
    st_asewkt,
    st_geohash,

    -- * Spatial relationships (additional)
    st_3dintersects,
    st_relate,
    st_orderingequals,
    st_dfullywithin,
    st_pointinsidecircle,

    -- * Measurement functions (additional)
    st_angle,
    st_closestpoint,
    st_3dclosestpoint,
    st_3ddistance,
    st_distancesphere,
    st_frechetdistance,
    st_hausdorffdistance,
    st_length2d,
    st_3dlength,
    st_longestline,
    st_3dlongestline,
    st_3dmaxdistance,
    st_minimumclearance,
    st_shortestline,
    st_3dshortestline,

    -- * Overlay functions
    st_symdifference,
    st_unaryunion,
    st_split,
    st_node,

    -- * Geometry processing
    st_union,
    st_unions,
    st_centroid,
    st_buffer,
    st_convexhull,
    st_envelope,
    st_pointonsurface,
    st_intersection,
    st_difference,
    st_buildarea,
    st_chaikinsmoothing,
    st_concavehull,
    st_delaunaytriangles,
    st_generatepoints,
    st_geometricmedian,
    st_linemerge,
    st_minimumboundingcircle,
    st_offsetcurve,
    st_reduceprecision,
    st_sharedpaths,
    st_simplify,
    st_simplifypreservetopology,
    st_voronoilines,
    st_voronoipolygons,

    -- * Affine transformations
    st_translate,
    st_scale,
    st_rotate,
    st_rotatex,
    st_rotatez,

    -- * Bounding box
    st_expand,

    -- * Linear referencing
    st_lineinterpolatepoint,
    st_linelocatepoint,
    st_linesubstring,

    -- * Points
    point,
    point_v,
    st_point,
    st_point_xyz,
    st_point_xyzm,

    -- * Transform
    st_transform_geography,
    st_transform_geometry,
    -- ** SRID
    SRID,
    wgs84,
    mercator,
    britishNationalGrid,
    SridUnit(..),

    -- * Other
    makePolygon,
    PostgisGeometry,
    HasPgType,

    -- * Re-exports
    PointXY(..),
    PointXYZ(..),
    PointXYZM(..),
  )
where

import Database.Esqueleto.Experimental(val)
import Data.Proxy
import Data.Bifunctor (first)
import Database.Esqueleto.Postgis.Ewkb (parseHexByteString)
import Data.Foldable (Foldable (toList), fold)
import Data.Geospatial (GeoPoint (..), GeoPositionWithoutCRS (..), GeospatialGeometry, PointXY (..), PointXYZ (..), PointXYZM (..))
import Data.Geospatial qualified as Geospatial
import Data.LineString (LineString, fromLineString, lineStringHead)
import Data.LinearRing (LinearRing, fromLinearRing, makeLinearRing, ringHead, toSeq)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as Non
import Data.Semigroup qualified as S
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text.Encoding(encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder qualified as Text
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction, unsafeSqlCastAs )
import Database.Persist.Sql
import Data.Base16.Types(assertBase16)
import Data.ByteString(fromStrict)

-- | unwrap postgis geometry so you can for example return it from an API
getPoints :: PostgisGeometry point -> NonEmpty point
getPoints geom = case geom of
  Point p -> p :| []
  MultiPoint pts -> pts
  Line ls -> linestringNonEmpty ls
  Multiline lss -> S.sconcat (fmap linestringNonEmpty lss)
  Polygon ring -> linearRingNonEmpty ring
  MultiPolygon rings -> S.sconcat (fmap linearRingNonEmpty rings)
  Collection geoms -> S.sconcat (fmap getPoints geoms)

linestringNonEmpty :: LineString a -> NonEmpty a
linestringNonEmpty ls = lineStringHead ls :| drop 1 (fromLineString ls)

linearRingNonEmpty :: LinearRing a -> NonEmpty a
linearRingNonEmpty ls = ringHead ls :| drop 1 (fromLinearRing ls)

tshow :: (Show a) => a -> Text
tshow = pack . show


-- | Guarantees we don't accidently mix curved space with flat space.
--   Postgis will catch this too, this just put's it in the type system.
data SpatialType = Geometry -- ^ assume a flat space.
                 | Geography -- ^ assume curvature of the earth.

-- | technical typeclass to bind a spatial type to a string value.
--   because we represent the constructors as a datakind, we need
--   this to go back to a value.
class HasPgType (spatialType :: SpatialType) where
  pgType :: Proxy spatialType -> Text

instance HasPgType 'Geometry where
  pgType _ = "geometry"

instance HasPgType 'Geography where
  pgType _ = "geography"

-- | backwards compatibility, initial version only dealt in geometry
type PostgisGeometry = Postgis 'Geometry


-- | like 'GeospatialGeometry' but not partial, eg no empty geometries.
--   Also can put an inveriant on dimensions if a function requires it.
--   for example 'st_intersects' 'PostgisGeometry' 'PointXY' can't work with 'PostgisGeometry' 'PointXYZ'.
--   PointXY indicates a 2 dimension space, and PointXYZ a three dimension space.
data Postgis (spatialType :: SpatialType) point
  = Point point
  | MultiPoint (NonEmpty point)
  | Line (LineString point)
  | Multiline (NonEmpty (LineString point))
  | Polygon (LinearRing point)
  | MultiPolygon (NonEmpty (LinearRing point))
  | Collection (NonEmpty (PostgisGeometry point))
  deriving (Show, Functor, Eq)

data GeomErrors
  = MismatchingDimensionsXYZ PointXYZ
  | MismatchingDimensionsXYZM PointXYZM
  | MismatchingDimensionsXY PointXY
  | NoGeometry
  | EmptyPoint
  | NotImplemented
  | EmptyMultiline
  | EmptyMultiPoint
  | NotEnoughElements
  | EmptyMultipolygon
  | EmptyCollection
  deriving (Show)

-- | checks if the first point is the last, and if not so makes it so.
--   this is required for inserting into the database
makePolygon :: (Eq point, Show point) => point -> point -> point -> Seq point -> LinearRing point
makePolygon one two three other =
  if Just one == last'
    then makeLinearRing one two three other
    else makeLinearRing one two three (other |> one)
  where
    last' = Seq.lookup (length other) other

from2dGeoPositionWithoutCRSToPoint :: GeoPositionWithoutCRS -> Either GeomErrors PointXY
from2dGeoPositionWithoutCRSToPoint = \case
  GeoEmpty -> Left EmptyPoint
  GeoPointXY x -> Right x
  GeoPointXYZ x -> Left (MismatchingDimensionsXYZ x)
  GeoPointXYZM x -> Left (MismatchingDimensionsXYZM x)

from3dGeoPositionWithoutCRSToPoint :: GeoPositionWithoutCRS -> Either GeomErrors PointXYZ
from3dGeoPositionWithoutCRSToPoint = \case
  GeoEmpty -> Left EmptyPoint
  GeoPointXY x -> Left (MismatchingDimensionsXY x)
  GeoPointXYZ x -> Right x
  GeoPointXYZM x -> Left (MismatchingDimensionsXYZM x)

from4dGeoPositionWithoutCRSToPoint :: GeoPositionWithoutCRS -> Either GeomErrors PointXYZM
from4dGeoPositionWithoutCRSToPoint = \case
  GeoEmpty -> Left EmptyPoint
  GeoPointXY x -> Left (MismatchingDimensionsXY x)
  GeoPointXYZ x -> Left (MismatchingDimensionsXYZ x)
  GeoPointXYZM x -> Right x

renderPair :: PointXY -> Text.Builder
renderPair (PointXY {..}) = fromString (show _xyX) <> " " <> fromString (show _xyY)

renderXYZ :: PointXYZ -> Text.Builder
renderXYZ (PointXYZ {..}) = fromString (show _xyzX) <> " " <> fromString (show _xyzY) <> " " <> fromString (show _xyzZ)

renderXYZM :: PointXYZM -> Text.Builder
renderXYZM (PointXYZM {..}) = fromString (show _xyzmX) <> " " <> fromString (show _xyzmY) <> " " <> fromString (show _xyzmZ) <> " " <> fromString (show _xyzmM)

renderGeometry :: forall (spatialType :: SpatialType) . HasPgType spatialType => Postgis spatialType Text.Builder -> Text.Builder
renderGeometry geom =
  let result = renderGeometryUntyped geom
  -- wrap it in quotes and cast it to whatever type we decided it should be
  in "'" <> result <> "' :: " <> (Text.fromText $ pgType $ (Proxy @spatialType) )

-- can't add quotes and types in the recursion because it's already part of the string
renderGeometryUntyped :: Postgis spatialType Text.Builder -> Text.Builder
renderGeometryUntyped = \case
  Point point' -> "POINT(" <> point' <> ")"
  MultiPoint points -> "MULTIPOINT (" <> fold (Non.intersperse "," ((\x -> "(" <> x <> ")") <$> points)) <> ")"
  Line line -> "LINESTRING(" <> renderLines line <> ")"
  Multiline multiline -> "MULTILINESTRING(" <> fold (Non.intersperse "," ((\line -> "(" <> renderLines line <> ")") <$> multiline)) <> ")"
  Polygon polygon -> "POLYGON((" <> renderLines polygon <> "))"
  MultiPolygon multipolygon -> "MULTIPOLYGON(" <> fold (Non.intersperse "," ((\line -> "((" <> renderLines line <> "))") <$> multipolygon)) <> ")"
  Collection collection -> "GEOMETRYCOLLECTION(" <> fold (Non.intersperse "," (renderGeometryUntyped <$> collection)) <> ")"



renderLines :: (Foldable f) => f Text.Builder -> Text.Builder
renderLines line = fold (List.intersperse "," $ toList line)

from2dGeospatialGeometry :: (Eq a, Show a) => (GeoPositionWithoutCRS -> Either GeomErrors a) -> GeospatialGeometry -> Either GeomErrors (Postgis spatialType a)
from2dGeospatialGeometry interpreter = \case
  Geospatial.NoGeometry -> Left NoGeometry
  Geospatial.Point (GeoPoint point') -> (Point <$> interpreter point')
  Geospatial.MultiPoint (Geospatial.GeoMultiPoint points) -> do
    list' <- sequence $ toList (interpreter <$> points)
    case nonEmpty list' of
      Nothing -> Left EmptyMultiPoint
      Just x -> Right $ MultiPoint x
  Geospatial.Line (Geospatial.GeoLine linestring) -> Line <$> traverse interpreter linestring
  Geospatial.MultiLine (Geospatial.GeoMultiLine multiline) -> do
    seqRes <- traverse (traverse interpreter) multiline
    case Non.nonEmpty (toList seqRes) of
      Just nonEmpty' -> Right $ Multiline nonEmpty'
      Nothing -> Left EmptyMultiline
  Geospatial.Polygon (Geospatial.GeoPolygon polygon) -> Polygon <$> (toLinearRing interpreter) polygon
  Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon multipolygon) -> do
    seqRings <- traverse (toLinearRing interpreter) multipolygon
    case Non.nonEmpty (toList seqRings) of
      Just nonEmpty' -> Right $ MultiPolygon nonEmpty'
      Nothing -> Left EmptyMultipolygon
  Geospatial.Collection seq' -> do
    seqs <- traverse (from2dGeospatialGeometry interpreter) seq'
    case Non.nonEmpty (toList seqs) of
      Just nonEmpty' -> Right $ Collection nonEmpty'
      Nothing -> Left EmptyCollection

toLinearRing :: (Eq a, Show a) => (GeoPositionWithoutCRS -> Either GeomErrors a) -> Seq (LinearRing GeoPositionWithoutCRS) -> Either GeomErrors (LinearRing a)
toLinearRing interpreter polygon = do
  aSeq <- traverse interpreter (foldMap toSeq polygon)
  case aSeq of
    (one :<| two :<| three :<| rem') -> Right $ makeLinearRing one two three rem'
    _other -> Left NotEnoughElements

instance HasPgType spatialType => PersistField (Postgis spatialType PointXY) where
  toPersistValue geom =
    PersistLiteral_ Unescaped $ encodeUtf8 $ toStrict $ toLazyText $ renderGeometry  $ renderPair <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString $ assertBase16 $ fromStrict bs
    first tshow $ (from2dGeospatialGeometry from2dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance HasPgType spatialType => PersistField (Postgis spatialType PointXYZ) where
  toPersistValue geom =
    PersistLiteral_ Unescaped $ encodeUtf8 $ toStrict $ toLazyText $ renderGeometry $ renderXYZ <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString $ assertBase16 $ fromStrict bs
    first tshow $ (from2dGeospatialGeometry from3dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance HasPgType spatialType => PersistField (Postgis spatialType PointXYZM) where
  toPersistValue geom =
    PersistLiteral_ Unescaped $ encodeUtf8 $ toStrict $ toLazyText $ renderGeometry $ renderXYZM <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString $ assertBase16 $ fromStrict bs
    first tshow $ (from2dGeospatialGeometry from4dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance forall spatialType . HasPgType spatialType => PersistFieldSql (Postgis spatialType PointXY) where
  sqlType _ = SqlOther $ pgType $ Proxy @spatialType

instance HasPgType spatialType => PersistFieldSql (Postgis spatialType PointXYZ) where
  sqlType _ = SqlOther $ pgType $ Proxy @spatialType

instance HasPgType spatialType => PersistFieldSql (Postgis spatialType PointXYZM) where
  sqlType _ = SqlOther $ pgType $ Proxy @spatialType


-- | Returns TRUE if geometry A contains geometry B.
--   https://postgis.net/docs/ST_Contains.html
st_contains ::
  -- | geom a
  SqlExpr (Value (Postgis 'Geometry a)) ->
  -- | geom b
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Bool)
st_contains a b = unsafeSqlFunction "ST_CONTAINS" (a, b)

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

-- | SRID
-- you can find your local like this: https://blog.rustprooflabs.com/2020/11/postgis-find-local-srid
-- geography appears to use 'wgs84'. So I hardcoded the use going from geom to geography as that.
--
-- you can use the num instance to put in whatever.
-- however, if you miss a srid please submit a PR.
newtype SRID (unit :: SridUnit) = SRID Int
  deriving newtype (Num, PersistField)

-- | default for geography type in postgis.
--   Geodetic CRS: WGS 84
--   https://epsg.io/4326
wgs84 :: SRID 'Degree
wgs84 = 4326

-- | most maps are in this, it has some large distortions further away from equator
--   https://epsg.io/3857
mercator :: SRID 'Linear
mercator = 3857

-- | if you're in england this is pretty good.
--   https://epsg.io/27700
britishNationalGrid :: SRID 'Linear
britishNationalGrid = 27700


-- | Diferent 'SRID' come in different units,
--   important for converting from geograhy to geometry.
data SridUnit = Linear -- ^ meters or feet
          | Degree -- ^ spheroids

-- | Project a geography onto a geometry.
--   allows using of functions such as 'st_union' which only work in flat space (geometry).
--   https://postgis.net/docs/ST_Transform.html
st_transform_geography :: forall a. SRID 'Linear ->
                        SqlExpr (Value (Postgis 'Geography a)) -> -- ^ g1 (library handles the conversion)
                        SqlExpr (Value (Postgis 'Geometry a))
st_transform_geography srid geography =
  unsafeSqlFunction "ST_Transform" (casted, val srid)
  where
    casted :: SqlExpr (Value (Postgis 'Geometry a))
    casted = unsafe_cast_pg_type geography

-- | project a geometry as a geography, assumes 'wgs84'.
--   https://postgis.net/docs/ST_Transform.html
st_transform_geometry :: SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ g1 (library handles the conversion)
                        SqlExpr (Value (Postgis 'Geography a))
st_transform_geometry input = unsafe_cast_pg_type transformed
  where
    transformed :: SqlExpr (Value (Postgis 'Geometry a))
    transformed =
      unsafeSqlFunction "ST_Transform" (input, val wgs84)

-- postgis doesn't appear to care about casting between spaces,
-- the user probably wants to use st_transform instead.
unsafe_cast_pg_type :: forall two one a . HasPgType two => SqlExpr (Value (Postgis one a)) -> SqlExpr (Value (Postgis two a))
unsafe_cast_pg_type = unsafeSqlCastAs castAs
  where
    castAs = pgType $ Proxy @two

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

-- | calculate the distance between two points
--   https://postgis.net/docs/ST_Distance.html
st_distance ::
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value (Postgis spatialType a)) ->
  SqlExpr (Value Double)
st_distance a b =
  unsafeSqlFunction "ST_distance" (a, b)

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

-- ---------------------------------------------------------------------------
-- Geometry Constructors
-- ---------------------------------------------------------------------------

-- | Aggregate function that collects geometries into a geometry collection.
--   https://postgis.net/docs/ST_Collect.html
st_collect ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_collect a = unsafeSqlFunction "ST_Collect" a

-- | Creates a rectangular polygon from minimum and maximum coordinates.
--   https://postgis.net/docs/ST_MakeEnvelope.html
st_makeenvelope ::
  SqlExpr (Value Double) -> -- ^ xmin
  SqlExpr (Value Double) -> -- ^ ymin
  SqlExpr (Value Double) -> -- ^ xmax
  SqlExpr (Value Double) -> -- ^ ymax
  SqlExpr (Value Int) ->    -- ^ srid
  SqlExpr (Value (Postgis 'Geometry a))
st_makeenvelope xmin' ymin' xmax' ymax' srid' = unsafeSqlFunction "ST_MakeEnvelope" (xmin', ymin', xmax', ymax', srid')

-- | Creates a linestring from two point geometries.
--   https://postgis.net/docs/ST_MakeLine.html
st_makeline ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_makeline a b = unsafeSqlFunction "ST_MakeLine" (a, b)

-- | Creates a polygon from a closed linestring shell.
--   Named st_makepolygon_line to avoid collision with 'makePolygon'.
--   https://postgis.net/docs/ST_MakePolygon.html
st_makepolygon_line ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_makepolygon_line a = unsafeSqlFunction "ST_MakePolygon" a

-- ---------------------------------------------------------------------------
-- Geometry Accessors (additional)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Geometry Editors
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Geometry Validation
-- ---------------------------------------------------------------------------

-- | Attempts to make an invalid geometry valid without losing vertices.
--   https://postgis.net/docs/ST_MakeValid.html
st_makevalid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_makevalid a = unsafeSqlFunction "ST_MakeValid" a

-- | Returns text describing why a geometry is invalid, or "Valid Geometry".
--   https://postgis.net/docs/ST_IsValidReason.html
st_isvalidreason ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_isvalidreason a = unsafeSqlFunction "ST_IsValidReason" a

-- ---------------------------------------------------------------------------
-- SRS Functions
-- ---------------------------------------------------------------------------

-- | Sets the SRID on a geometry to a particular integer value.
--   https://postgis.net/docs/ST_SetSRID.html
st_setsrid ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Int) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_setsrid a srid' = unsafeSqlFunction "ST_SetSRID" (a, srid')

-- ---------------------------------------------------------------------------
-- Geometry Output
-- ---------------------------------------------------------------------------

-- | Returns the Well-Known Text (WKT) representation of the geometry.
--   https://postgis.net/docs/ST_AsText.html
st_astext ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_astext a = unsafeSqlFunction "ST_AsText" a

-- | Returns the GeoJSON representation of the geometry.
--   https://postgis.net/docs/ST_AsGeoJSON.html
st_asgeojson ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_asgeojson a = unsafeSqlFunction "ST_AsGeoJSON" a

-- | Returns the Extended Well-Known Text (EWKT) representation of the geometry.
--   https://postgis.net/docs/ST_AsEWKT.html
st_asewkt ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_asewkt a = unsafeSqlFunction "ST_AsEWKT" a

-- | Returns a GeoHash representation of the geometry.
--   https://postgis.net/docs/ST_GeoHash.html
st_geohash ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Text)
st_geohash a = unsafeSqlFunction "ST_GeoHash" a

-- ---------------------------------------------------------------------------
-- Spatial Relationships (additional)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Measurement Functions (additional)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Overlay Functions
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Geometry Processing (additional)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Affine Transformations
-- ---------------------------------------------------------------------------

-- | Translates a geometry by given X and Y offsets.
--   https://postgis.net/docs/ST_Translate.html
st_translate ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ deltaX
  SqlExpr (Value Double) -> -- ^ deltaY
  SqlExpr (Value (Postgis 'Geometry a))
st_translate a dx dy = unsafeSqlFunction "ST_Translate" (a, dx, dy)

-- | Scales a geometry by given X and Y factors.
--   https://postgis.net/docs/ST_Scale.html
st_scale ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ scaleX
  SqlExpr (Value Double) -> -- ^ scaleY
  SqlExpr (Value (Postgis 'Geometry a))
st_scale a sx sy = unsafeSqlFunction "ST_Scale" (a, sx, sy)

-- | Rotates a geometry around the origin by an angle in radians.
--   https://postgis.net/docs/ST_Rotate.html
st_rotate ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ angle in radians
  SqlExpr (Value (Postgis 'Geometry a))
st_rotate a angle = unsafeSqlFunction "ST_Rotate" (a, angle)

-- | Rotates a geometry around the X axis by an angle in radians.
--   https://postgis.net/docs/ST_RotateX.html
st_rotatex ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ angle in radians
  SqlExpr (Value (Postgis 'Geometry a))
st_rotatex a angle = unsafeSqlFunction "ST_RotateX" (a, angle)

-- | Rotates a geometry around the Z axis by an angle in radians.
--   https://postgis.net/docs/ST_RotateZ.html
st_rotatez ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ angle in radians
  SqlExpr (Value (Postgis 'Geometry a))
st_rotatez a angle = unsafeSqlFunction "ST_RotateZ" (a, angle)

-- ---------------------------------------------------------------------------
-- Bounding Box
-- ---------------------------------------------------------------------------

-- | Returns a bounding box expanded in all directions by a given distance.
--   https://postgis.net/docs/ST_Expand.html
st_expand ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) ->
  SqlExpr (Value (Postgis 'Geometry a))
st_expand a d = unsafeSqlFunction "ST_Expand" (a, d)

-- ---------------------------------------------------------------------------
-- Linear Referencing
-- ---------------------------------------------------------------------------

-- | Returns a point interpolated along a line at a fractional distance (0.0 to 1.0).
--   https://postgis.net/docs/ST_LineInterpolatePoint.html
st_lineinterpolatepoint ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ fraction (0.0 to 1.0)
  SqlExpr (Value (Postgis 'Geometry a))
st_lineinterpolatepoint a f = unsafeSqlFunction "ST_LineInterpolatePoint" (a, f)

-- | Returns a float between 0 and 1 representing the location of the closest point on a line to a given point.
--   https://postgis.net/docs/ST_LineLocatePoint.html
st_linelocatepoint ::
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ line
  SqlExpr (Value (Postgis 'Geometry a)) -> -- ^ point
  SqlExpr (Value Double)
st_linelocatepoint a b = unsafeSqlFunction "ST_LineLocatePoint" (a, b)

-- | Returns the portion of a line between two fractional locations (0.0 to 1.0).
--   https://postgis.net/docs/ST_LineSubstring.html
st_linesubstring ::
  SqlExpr (Value (Postgis 'Geometry a)) ->
  SqlExpr (Value Double) -> -- ^ start fraction
  SqlExpr (Value Double) -> -- ^ end fraction
  SqlExpr (Value (Postgis 'Geometry a))
st_linesubstring a s e = unsafeSqlFunction "ST_LineSubstring" (a, s, e)

point ::
  Double -> -- ^ x or longitude
  Double -> -- ^ y or latitude
  (Postgis spatialType PointXY)
point x y = Point (PointXY {_xyX = x, _xyY = y})

point_v :: HasPgType spatialType =>
  Double -> -- ^ x or longitude
  Double -> -- ^ y or latitude
  SqlExpr (Value (Postgis spatialType PointXY))
point_v = fmap val . point

st_point ::  forall spatialType . HasPgType spatialType =>
  SqlExpr (Value Double) -> -- ^ x or longitude
  SqlExpr (Value Double) -> -- ^ y or latitude
  SqlExpr (Value (Postgis spatialType PointXY))
st_point a b = unsafeSqlCastAs castAs $ unsafeSqlFunction "ST_POINT" (a, b)
  where
    castAs = pgType $ Proxy @spatialType


st_point_xyz ::  forall spatialType . HasPgType spatialType =>
  SqlExpr (Value Double) -> -- ^ x or longitude
  SqlExpr (Value Double) -> -- ^ y or latitude
  SqlExpr (Value Double) -> -- ^ z elevation/altitude
  SqlExpr (Value (Postgis spatialType PointXYZ))
st_point_xyz a b c = unsafeSqlCastAs castAs $ unsafeSqlFunction "ST_POINT" (a, b, c)
  where
    castAs = pgType $ Proxy @spatialType

st_point_xyzm :: forall spatialType . HasPgType spatialType =>
  SqlExpr (Value Double) -> -- ^ x or longitude
  SqlExpr (Value Double) -> -- ^ y or latitude
  SqlExpr (Value Double) -> -- ^ z elevation/altitude
  SqlExpr (Value Double) -> -- ^ m measure, user defined dimension
  SqlExpr (Value (Postgis spatialType PointXYZM))
st_point_xyzm a b c m = unsafeSqlCastAs castAs $ unsafeSqlFunction "ST_POINT" (a, b, c, m)
  where
    castAs = pgType $ Proxy @spatialType
