{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell bindings for postgres postgis
--   for a good explenation see <https://postgis.net/>
module Database.Esqueleto.Postgis
  ( PostgisGeometry (..),
    makePolygon,
    getPoints,

    -- * functions
    st_contains,
    st_intersects,
    st_union,

    -- * points
    point,
    st_point,
    st_point_xyz,
    st_point_xyzm,

    -- * re-exports
    PointXY(..),
    PointXYZ(..),
    PointXYZM(..),
  )
where

import Data.Bifunctor (first)
import Database.Esqueleto.Postgis.Ewkb (parseHexByteString)
import Data.Foldable (Foldable (toList), fold)
import Data.Geospatial (GeoPoint (..), GeoPositionWithoutCRS (..), GeospatialGeometry, PointXY (..), PointXYZ (..), PointXYZM (..))
import Data.Geospatial qualified as Geospatial
import Database.Esqueleto.Postgis.Hex (Hex (..))
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
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder qualified as Text
import Database.Esqueleto.Experimental (SqlExpr, Value)
import Database.Esqueleto.Internal.Internal (unsafeSqlFunction)
import Database.Persist.Sql

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

-- | like 'GeospatialGeometry' but not partial, eg no empty geometries, also only works
--   in a single dimention, eg PostgisGeometry PointXY can't work with PostgisGeometry PointXYZ.
--   so PointXY indicates a 2 dimension space, and PointXYZ a three dimension space.
data PostgisGeometry point
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

renderGeometry :: PostgisGeometry Text.Builder -> Text.Builder
renderGeometry = \case
  Point point' -> "POINT(" <> point' <> ")"
  MultiPoint points -> "MULTIPOINT (" <> fold (Non.intersperse "," ((\x -> "(" <> x <> ")") <$> points)) <> ")"
  Line line -> "LINESTRING(" <> renderLines line <> ")"
  Multiline multiline -> "MULTILINESTRING(" <> fold (Non.intersperse "," ((\line -> "(" <> renderLines line <> ")") <$> multiline)) <> ")"
  Polygon polygon -> "POLYGON((" <> renderLines polygon <> "))"
  MultiPolygon multipolygon -> "MULTIPOLYGON(" <> fold (Non.intersperse "," ((\line -> "((" <> renderLines line <> "))") <$> multipolygon)) <> ")"
  Collection collection -> "GEOMETRYCOLLECTION(" <> fold (Non.intersperse "," (renderGeometry <$> collection)) <> ")"

extractFirst :: PostgisGeometry a -> a
extractFirst = \case
  Point point' -> point'
  MultiPoint points -> Non.head points
  Line line -> lineStringHead line
  Multiline multiline -> lineStringHead $ Non.head multiline
  Polygon polygon -> ringHead polygon
  MultiPolygon multipolygon -> ringHead $ Non.head multipolygon
  Collection collection -> extractFirst $ Non.head collection

renderLines :: (Foldable f) => f Text.Builder -> Text.Builder
renderLines line = fold (List.intersperse "," $ toList line)

from2dGeospatialGeometry :: (Eq a, Show a) => (GeoPositionWithoutCRS -> Either GeomErrors a) -> GeospatialGeometry -> Either GeomErrors (PostgisGeometry a)
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

instance PersistField PointXY where
  toPersistValue geom = toPersistValue (Point geom)
  fromPersistValue x = extractFirst <$> fromPersistValue x

instance PersistFieldSql PointXY where
  sqlType _ = SqlOther "geometry"

instance PersistField (PostgisGeometry PointXY) where
  toPersistValue geom =
    PersistText $ toStrict $ toLazyText $ renderGeometry $ renderPair <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString (Hex bs)
    first tshow $ (from2dGeospatialGeometry from2dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance PersistField (PostgisGeometry PointXYZ) where
  toPersistValue geom =
    PersistText $ toStrict $ toLazyText $ renderGeometry $ renderXYZ <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString (Hex bs)
    first tshow $ (from2dGeospatialGeometry from3dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance PersistField (PostgisGeometry PointXYZM) where
  toPersistValue geom =
    PersistText $ toStrict $ toLazyText $ renderGeometry $ renderXYZM <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
    result <- first pack $ parseHexByteString (Hex bs)
    first tshow $ (from2dGeospatialGeometry from4dGeoPositionWithoutCRSToPoint) result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance PersistFieldSql (PostgisGeometry PointXY) where
  sqlType _ = SqlOther "geometry"

instance PersistFieldSql (PostgisGeometry PointXYZ) where
  sqlType _ = SqlOther "geometry"

instance PersistFieldSql (PostgisGeometry PointXYZM) where
  sqlType _ = SqlOther "geometry"

-- | Returns TRUE if geometry A contains geometry B.
--   https://postgis.net/docs/ST_Contains.html
st_contains ::
  -- | geom a
  SqlExpr (Value (PostgisGeometry a)) ->
  -- | geom b
  SqlExpr (Value (PostgisGeometry a)) ->
  SqlExpr (Value Bool)
st_contains a b = unsafeSqlFunction "ST_CONTAINS" (a, b)

-- | allows union of geometries, eg group a bunch together, for example:
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
  SqlExpr (Value (PostgisGeometry a)) ->
  SqlExpr (Value (PostgisGeometry a))
st_union a = unsafeSqlFunction "ST_union" a

-- | Returns true if two geometries intersect.
--   Geometries intersect if they have any point in common.
--   https://postgis.net/docs/ST_Intersects.html
st_intersects ::
  SqlExpr (Value (PostgisGeometry a)) ->
  SqlExpr (Value (PostgisGeometry a)) ->
  SqlExpr (Value Bool)
st_intersects a b = unsafeSqlFunction "ST_Intersects" (a, b)

point :: Double -> Double -> (PostgisGeometry PointXY)
point x y = Point (PointXY {_xyX = x, _xyY = y})

st_point :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (PostgisGeometry PointXY))
st_point a b = unsafeSqlFunction "ST_POINT" (a, b)

st_point_xyz :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (PostgisGeometry PointXYZ))
st_point_xyz a b c = unsafeSqlFunction "ST_POINT" (a, b, c)

st_point_xyzm :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (PostgisGeometry PointXYZM))
st_point_xyzm a b c m = unsafeSqlFunction "ST_POINT" (a, b, c, m)
