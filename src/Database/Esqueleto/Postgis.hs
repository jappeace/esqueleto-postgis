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

    -- * Spatial relationships
    module Database.Esqueleto.Postgis.Spatial,

    -- * Measurement functions
    module Database.Esqueleto.Postgis.Measurement,

    -- * Geometry accessors
    module Database.Esqueleto.Postgis.Accessor,

    -- * Geometry constructors
    st_collect,
    st_makeenvelope,
    st_makeline,
    st_makepolygon_line,

    -- * Geometry editors
    module Database.Esqueleto.Postgis.Editor,

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

    -- * Geometry processing
    module Database.Esqueleto.Postgis.Processing,

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

import Database.Esqueleto.Postgis.Geometry (Postgis(..), SpatialType(..), HasPgType(..), PostgisGeometry)
import Database.Esqueleto.Postgis.Spatial
import Database.Esqueleto.Postgis.Measurement
import Database.Esqueleto.Postgis.Accessor
import Database.Esqueleto.Postgis.Editor
import Database.Esqueleto.Postgis.Processing

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
