--  I copied these because the original libraryy doesn't
--   appear to be maintained.
--   I'm only interested in this parseHexBytestring function.
--
--   friendly copied from github.com/zellige/wkt-geom.git
--   Copyright 2017-2018 wkt-geom Project
--   Apache license
--
--
-- | Ewkb is the basis for postgis in postgres.
--  This module  Allows parsing of ByteString into a Geospatial Object.
--
-- These functions are used in the instance 'Database.Persist.Class.PersistField' instance
-- to parse the database base16 output into types we can use.
--
-- Refer to the eWKB Postgis Documentation <https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT>
--
module Database.Esqueleto.Postgis.Ewkb
  ( parseByteString
  , parseHexByteString
  , toByteString
  -- * Core
  -- | the greasy gears inside for experienced users or the brave!
  , module EwkbGeometry
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial

import Database.Esqueleto.Postgis.Ewkb.Geometry  as EwkbGeometry
import qualified Database.Esqueleto.Postgis.Wkb.Endian     as Endian
import qualified Database.Esqueleto.Postgis.Wkb.Geospatial as WkbGeospatial
import Data.ByteString.Lazy.Base16(decodeBase16)
import Data.Base16.Types(Base16)
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
parseHexByteString :: Base16 LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseHexByteString = parseByteString . decodeBase16

-- |
-- Produce the binary representation of EWKB given its EndianType (Little or Big - Intel is Little) and SRID (4326 for example).
toByteString :: Endian.EndianType -> EwkbGeometry.SridType -> Geospatial.GeospatialGeometry -> LazyByteString.ByteString
toByteString endianType sridType =
  ByteStringBuilder.toLazyByteString . WkbGeospatial.builderGeospatialGeometry
    mkBuilder endianType
  where mkBuilder eType gType = EwkbGeometry.builderEwkbGeom eType (EwkbGeometry.EwkbGeom gType sridType)
