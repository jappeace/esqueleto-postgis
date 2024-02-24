{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
-- | Haskell bindings for postgres postgis
--   for a good explenation see <https://postgis.net/>
module Database.Esqueleto.Postgis (
  st_contains,
  st_point,
  -- st_polygon
  PostgisGeometry(..)
  , makePolygon
) where

import Data.Sequence qualified as Seq
import Data.Sequence(Seq(..), (|>))
import Data.Geospatial(GeospatialGeometry, GeoPoint (..))
import Data.Ewkb(parseHexByteString)
import Data.Hex(Hex(..))
import Database.Esqueleto.Experimental(SqlExpr, Value)
import Database.Persist.Sql
import Database.Esqueleto.Internal.Internal(unsafeSqlFunction)
import Data.Text(pack, Text)
import Data.Bifunctor(first)
import Data.LineString (LineString)
import GHC.Base (NonEmpty)
import Data.LinearRing (LinearRing, makeLinearRing, toSeq)
import Data.Geospatial.Internal.BasicTypes (PointXY)
import Data.Geospatial (GeoPositionWithoutCRS)
import Data.Geospatial (GeoPositionWithoutCRS(..))
import Data.Geospatial (PointXYZM)
import Data.Geospatial (PointXYZ)
import Data.Geospatial qualified as Geospatial
import Data.Geospatial (PointXY(..))
import qualified Data.Text.Lazy.Builder as Text
import Data.String (IsString(..))
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import qualified Data.List.NonEmpty as Non
import Data.Foldable (fold, Foldable (toList))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List as List

tshow :: Show a => a -> Text
tshow = pack . show

-- | like 'GeospatialGeometry' but not partial, eg no empty geometries, also only works
--   in a single dimention, eg PostgisGeometry PointXY can't work with PostgisGeometry PointXYZ.
--   so PointXY indicates a 2 dimension space, and PointXYZ a three dimension space.
data PostgisGeometry point = Point point
                      | MultiPoint (NonEmpty point )
                      | Line (LineString point )
                      | Multiline (NonEmpty (LineString point))
                      | Polygon (LinearRing point )
                      | MultiPolygon (NonEmpty (LinearRing point ))
                      | Collection (NonEmpty (PostgisGeometry point))
                      deriving (Show, Functor, Eq)

data GeomErrors = MismatchingDimensionsXYZ PointXYZ
                | MismatchingDimensionsXYZM PointXYZM
                | NoGeometry
                | EmptyPoint
                | NotImplemented
                | EmptyMultiline
                | EmptyMultiPoint
                | NotEnoughElements (Seq PointXY)
                | EmptyMultipolygon
                | EmptyCollection
                deriving Show

-- | checks if the first point is the last, and if not so makes it so.
--   this is required for inserting into the database
makePolygon :: (Eq point, Show point) => point -> point -> point -> Seq point -> LinearRing point
makePolygon one two three other =
  if Just one == last then
    makeLinearRing  one two three other
  else
    makeLinearRing  one two three (other |> one)
  where
    last = Seq.lookup (length other) other

from2dGeoPositionWithoutCRSToPoint :: GeoPositionWithoutCRS -> Either GeomErrors PointXY
from2dGeoPositionWithoutCRSToPoint = \case
  GeoEmpty -> Left EmptyPoint
  GeoPointXY x -> Right x
  GeoPointXYZ x -> Left (MismatchingDimensionsXYZ x)
  GeoPointXYZM x -> Left (MismatchingDimensionsXYZM x)

renderPair :: PointXY -> Text.Builder
renderPair (PointXY {..}) = fromString (show _xyX) <> " " <> fromString (show _xyY)

renderGeometry :: PostgisGeometry Text.Builder -> Text.Builder
renderGeometry = \case
  Point point -> "POINT(" <> point <> ")"
  MultiPoint points -> "MULTIPOINT (" <> fold (Non.intersperse "," ((\x -> "(" <> x <> ")" ) <$> points)) <> ")"
  Line line -> "LINESTRING(" <> renderLines line <> ")"
  Multiline (multiline) -> "MULTILINESTRING(" <> fold (Non.intersperse "," ((\ line -> "(" <> renderLines line <> ")") <$> multiline)) <>")"
  Polygon polygon -> "POLYGON((" <> renderLines polygon <> "))"
  MultiPolygon multipolygon -> "MULTIPOLYGON(" <> fold (Non.intersperse "," ((\ line -> "((" <> renderLines line <> "))") <$> multipolygon))<> ")"
  Collection collection -> "GEOMETRYCOLLECTION("<> fold (Non.intersperse "," (renderGeometry <$> collection)) <>  ")"

renderLines :: Foldable f => f Text.Builder -> Text.Builder
renderLines line = fold (List.intersperse "," $ toList line)

from2dGeospatialGeometry :: GeospatialGeometry -> Either GeomErrors (PostgisGeometry PointXY)
from2dGeospatialGeometry = \case
  Geospatial.NoGeometry -> Left NoGeometry
  Geospatial.Point (GeoPoint point) -> (Point <$> from2dGeoPositionWithoutCRSToPoint point)
  Geospatial.MultiPoint (Geospatial.GeoMultiPoint points) -> do
    list' <- sequence $ toList (from2dGeoPositionWithoutCRSToPoint <$> points)
    case nonEmpty list' of
      Nothing -> Left EmptyMultiPoint
      Just x -> Right $ MultiPoint x
  Geospatial.Line (Geospatial.GeoLine linestring) -> Line <$> traverse from2dGeoPositionWithoutCRSToPoint linestring
  Geospatial.MultiLine (Geospatial.GeoMultiLine multiline) -> do
    seqRes <- traverse (traverse from2dGeoPositionWithoutCRSToPoint) multiline
    case Non.nonEmpty (toList seqRes) of
      Just nonEmpty -> Right $ Multiline nonEmpty
      Nothing -> Left EmptyMultiline
  Geospatial.Polygon (Geospatial.GeoPolygon polygon) -> Polygon <$> toLinearRing polygon
  Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon multipolygon) -> do
    seqRings <- traverse toLinearRing multipolygon
    case Non.nonEmpty (toList seqRings ) of
      Just nonEmpty -> Right $ MultiPolygon nonEmpty
      Nothing -> Left EmptyMultipolygon

  Geospatial.Collection seq -> do
    seqs <- traverse from2dGeospatialGeometry seq
    case Non.nonEmpty (toList seqs ) of
      Just nonEmpty -> Right $ Collection nonEmpty
      Nothing -> Left EmptyCollection

toLinearRing ::  Seq (LinearRing GeoPositionWithoutCRS) -> Either GeomErrors (LinearRing PointXY)
toLinearRing polygon = do
    aSeq <- traverse from2dGeoPositionWithoutCRSToPoint (foldMap toSeq polygon)
    case aSeq of
      (one :<| two :<| three :<| rem) -> Right $ makeLinearRing  one two three rem
      other -> Left (NotEnoughElements other)

instance PersistField (PostgisGeometry PointXY) where
  toPersistValue geom =
      PersistText $ toStrict $ toLazyText $ renderGeometry $ renderPair <$> geom
  fromPersistValue (PersistLiteral_ Escaped bs) = do
      result <- first pack $ parseHexByteString (Hex bs)
      first tshow $ from2dGeospatialGeometry result
  fromPersistValue other = Left ("PersistField.Polygon: invalid persist value:" <> tshow other)

instance PersistFieldSql (PostgisGeometry PointXY) where
  sqlType _ = SqlOther "geometry"

st_contains :: SqlExpr (Value (PostgisGeometry a)) -> SqlExpr (Value (PostgisGeometry a)) -> SqlExpr (Value Bool)
st_contains a b = unsafeSqlFunction "ST_CONTAINS" (a,b)

st_point :: SqlExpr (Value Double) -> SqlExpr (Value Double) -> SqlExpr (Value (PostgisGeometry PointXY))
st_point a b = unsafeSqlFunction "ST_POINT" (a,b)

-- st_polygon
