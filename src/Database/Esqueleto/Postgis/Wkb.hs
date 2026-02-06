-- | Ewkb is deeply integrated into Wkb, which is why we kept this around.
-- For postgis we mostly want Ewkb.
--
-- This module Allows parsing of ByteString into a Geospatial Object,
-- and provides foundational elements for constructing an Ewkb.
--
-- Refer to the WKB Wikipedia page <https://en.wikipedia.org/wiki/Well-known_text#Well-known_binary>
module Database.Esqueleto.Postgis.Wkb
  ( parseByteString
  , parseHexByteString
  , toByteString
  -- * Core
  -- | the greasy gears inside for experienced users or the brave!

  -- ** Geometry
  , module Geometry
  -- ** Endian
  , module Endian
  -- ** Point
  , module Point
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial

import Database.Esqueleto.Postgis.Wkb.Endian     as Endian
import Database.Esqueleto.Postgis.Wkb.Geometry   as Geometry
import Database.Esqueleto.Postgis.Wkb.Point      as Point
import qualified Database.Esqueleto.Postgis.Wkb.Geospatial as WkbGeospatial
import Data.ByteString.Lazy.Base16(decodeBase16)
import Data.Base16.Types(Base16)

-- |
-- Representation of WKB as Binary
parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.getGeospatialGeometry Geometry.getWkbGeom)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse wkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry

-- |
-- Representation of WKB as a String in Base16/Hex form i.e. "0101000000000000000000f03f0000000000000040" is POINT 1.0 2.0
parseHexByteString :: Base16 LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseHexByteString = parseByteString . decodeBase16

-- |
-- Produce the binary representation of WKB given its EndianType (Little or Big - Intel is Little).  Use EWKB when you know the SRID.
toByteString :: Endian.EndianType -> Geospatial.GeospatialGeometry -> LazyByteString.ByteString
toByteString endianType =
  ByteStringBuilder.toLazyByteString . WkbGeospatial.builderGeospatialGeometry Geometry.builderWkbGeom endianType
