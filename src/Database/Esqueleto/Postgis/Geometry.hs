{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Database.Esqueleto.Postgis.Geometry
  ( SpatialType(..)
  , HasPgType(..)
  , Postgis(..)
  , PostgisGeometry
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.LineString (LineString)
import Data.LinearRing (LinearRing)
import Data.Text (Text)

-- | Guarantees we don't accidently mix curved space with flat space.
--   Postgis will catch this too, this just put's it in the type system.
data SpatialType = Geometry -- ^ assume a flat space.
                 | Geography -- ^ assume curvature of the earth.

-- | technical typeclass to bind a spatial type to a string value.
--   because we represent the constructors as a datakind, we need
--   this to go back to a value.
class HasPgType (spatialType :: SpatialType) where
  pgType :: proxy spatialType -> Text

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
