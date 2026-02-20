{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell bindings for postgres postgis
--   for a good explenation see <https://postgis.net/>
module Database.Esqueleto.Postgis
  (
    Postgis(..),
    SpatialType(..),
    getPoints,

    -- * functions
    st_contains,
    st_intersects,
    st_union,
    st_unions,
    st_dwithin,
    st_distance         ,

    -- * points
    point,
    point_v,
    st_point,
    st_point_xyz,
    st_point_xyzm,

    -- * transform
    st_transform_geography,
    st_transform_geometry,
    -- ** srid
    SRID,
    wgs84,
    mercator ,
    britishNationalGrid,
    SridUnit    (..),


    -- * other
    makePolygon,
    PostgisGeometry,
    HasPgType,

    -- * re-exports
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


-- | Spatial Reference System.
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

point :: Double -> Double -> (Postgis spatialType PointXY)
point x y = Point (PointXY {_xyX = x, _xyY = y})

point_v :: HasPgType spatialType => Double -> Double -> SqlExpr (Value (Postgis spatialType PointXY))
point_v = fmap val . point

st_point :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (Postgis spatialType PointXY))
st_point a b = unsafeSqlFunction "ST_POINT" (a, b)

st_point_xyz :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (Postgis spatialType PointXYZ))
st_point_xyz a b c = unsafeSqlFunction "ST_POINT" (a, b, c)

st_point_xyzm :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (Postgis spatialType PointXYZM))
st_point_xyzm a b c m = unsafeSqlFunction "ST_POINT" (a, b, c, m)

