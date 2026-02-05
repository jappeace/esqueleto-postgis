-- | I copied these because the original libraryy doesn't
--   appear to be maintained.
--   I'm only interested in this parseHexBytestring function.
--
--   friendly copied from github.com/zellige/wkt-geom.git
--   Copyright 2017-2018 wkt-geom Project
--   Apache license
--
--
-- Refer to the eWKB Postgis Documentation <https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT>
--
-- Allows parsing of ByteString into a Geospatial Object.
--
-------------------------------------------------------------------
module Database.Esqueleto.Postgis.Ewkb
  ( parseByteString
  , parseHexByteString
  , toByteString
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial
import qualified Database.Esqueleto.Postgis.Hex  as Hex

import qualified Database.Esqueleto.Postgis.Ewkb.Geometry  as EwkbGeometry
import qualified Database.Esqueleto.Postgis.Wkb.Endian     as Endian
import qualified Database.Esqueleto.Postgis.Wkb.Geospatial as WkbGeospatial

-- |
-- Representation of EWKB as Binary
parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.getGeospatialGeometry EwkbGeometry.getWkbGeom)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse ewkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry

-- |
-- Representation of EWKB as a String in Base16/Hex form i.e. "0101000000000000000000f03f0000000000000040" is POINT 1.0 2.0
parseHexByteString :: Hex.Hex -> Either String Geospatial.GeospatialGeometry
parseHexByteString = Hex.safeConvert parseByteString

-- |
-- Produce the binary representation of EWKB given its EndianType (Little or Big - Intel is Little) and SRID (4326 for example).
toByteString :: Endian.EndianType -> EwkbGeometry.SridType -> Geospatial.GeospatialGeometry -> LazyByteString.ByteString
toByteString endianType sridType =
  ByteStringBuilder.toLazyByteString . WkbGeospatial.builderGeospatialGeometry
    mkBuilder endianType
  where mkBuilder eType gType = EwkbGeometry.builderEwkbGeom eType (EwkbGeometry.EwkbGeom gType sridType)
