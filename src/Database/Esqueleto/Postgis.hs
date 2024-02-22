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
) where

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
import Data.LinearRing (LinearRing)
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

tshow :: Show a => a -> Text
tshow = pack . show

-- | like 'GeospatialGeometry' but not partial, eg no empty geometries, also only works
--   in a single dimention, eg PostgisGeometry PointXY can't work with PostgisGeometry PointXYZ.
--   so PointXY indicates a 2 dimension space, and PointXYZ a three dimension space.
data PostgisGeometry point = Point point
                      | MultiPoint (NonEmpty point )
                      | Polygon (LinearRing point )
                      | MultiPolygon (NonEmpty (LinearRing point ))
                      | Line (LineString point )
                      | Multiline (NonEmpty (LineString point))
                      | Collection (NonEmpty (PostgisGeometry point))
                      deriving (Show, Functor, Eq)

data GeomErrors = MismatchingDimensionsXYZ PointXYZ
                | MismatchingDimensionsXYZM PointXYZM
                | NoGeometry
                | EmptyPoint
                | NotImplemented
                deriving Show

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

from2dGeospatialGeometry :: GeospatialGeometry -> Either GeomErrors (PostgisGeometry PointXY)
from2dGeospatialGeometry = \case
  Geospatial.NoGeometry -> Left NoGeometry
  Geospatial.Point (GeoPoint point) -> (Point <$> from2dGeoPositionWithoutCRSToPoint point)
  Geospatial.MultiPoint (Geospatial.GeoMultiPoint points) -> do
    list' <- sequence $ toList (from2dGeoPositionWithoutCRSToPoint <$> points)
    case nonEmpty list' of
      Nothing -> Left NoGeometry
      Just x -> Right $ MultiPoint x
  _ -> Left NotImplemented


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
