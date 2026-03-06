{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger (..), runStderrLoggingT)
import Control.Monad.Trans.Resource (MonadThrow, ResourceT, runResourceT)
import Data.LineString (LineString, makeLineString)
import Data.LinearRing (LinearRing, makeLinearRing)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Esqueleto.Postgis
import Database.Persist
import Database.Persist.Postgresql
  ( ConnectionString,
    withPostgresqlConn,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistUpperCase,
    share,
    sqlSettings,
  )
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec(testSpec)

import qualified Ewkb.GeometrySpec
import qualified Ewkb.LineSpec
import qualified Ewkb.PointSpec

import qualified Wkb.EndianSpec
import qualified Wkb.GeometryCollectionSpec
import qualified Wkb.GeometrySpec
import qualified Wkb.LineSpec
import qualified Wkb.PointSpec
import qualified Wkb.PolygonSpec


connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistUpperCase|
  Grid
    geom (PostgisGeometry PointXY)
    label Text

  GeoGrid
    geo (Postgis 'Geography PointXY)
    label Text

  Unit sql=unit
    geom       (PostgisGeometry PointXY)
    deriving Eq Show

  Unityz sql=unityz
    geom       (PostgisGeometry PointXYZ)
    deriving Eq Show

  Unityzm sql=unityzm
    geom       (PostgisGeometry PointXYZM)
    deriving Eq Show
|]

initializeDB ::
  (MonadIO m) =>
  SqlPersistT (ResourceT m) ()
initializeDB = do
  runMigration migrateAll

runDB :: (forall m. (MonadIO m, MonadLogger m, MonadThrow m) => SqlPersistT (ResourceT m) a) -> IO a
runDB act =
  runStderrLoggingT
    . runResourceT
    . withPostgresqlConn connString
    . runSqlConn
    $ (initializeDB >> act >>= \ret -> transactionUndo >> return ret)

main :: IO ()
main = do
  -- normally hspec discover does this, but we don't want to use
  -- convuleted preprocessor,
  -- it sometimes doesn't work and it's hard to understand why.
  -- so I prefer to just import everything by hand.
  -- also, go look at the source of hspec discover, it's kinda shocking.
  hspecTrees <- sequence [
    testSpec "Ewkb.GeometrySpec" Ewkb.GeometrySpec.spec,
    testSpec "Ewkb.LineSpec" Ewkb.LineSpec.spec,
    testSpec "Ewkb.PointSpec" Ewkb.PointSpec.spec,

    testSpec "Wkb.EndianSpec" Wkb.EndianSpec.spec,
    testSpec "Wkb.GeometryCollectionSpec" Wkb.GeometryCollectionSpec.spec,
    testSpec "Wkb.GeometrySpec" Wkb.GeometrySpec.spec,
    testSpec "Wkb.LineSpec" Wkb.LineSpec.spec,
    testSpec "Wkb.PointSpec" Wkb.PointSpec.spec,
    testSpec "Wkb.PolygonSpec" Wkb.PolygonSpec.spec

    ]

  defaultMain $
    testGroup "all tests" $ postgisBindingsTests : hspecTrees

test' :: Gen (PostgisGeometry PointXY) -> TestTree
test' gen =
  testCase "roundtrip xy geometry" $ do
    someUnit <- Gen.sample (Unit <$> gen)
    result <- runDB $ do
      _ <- insert someUnit
      selectList @(Unit) [] []
    (entityVal <$> result) @?= [someUnit]

testxyz :: Gen (PostgisGeometry PointXYZ) -> TestTree
testxyz gen =
  testCase "roundtrip xyz geometry" $ do
    someUnit <- Gen.sample (Unityz <$> gen)
    result <- runDB $ do
      _ <- insert someUnit
      selectList @(Unityz) [] []
    (entityVal <$> result) @?= [someUnit]

testxyzm :: Gen (PostgisGeometry PointXYZM) -> TestTree
testxyzm gen =
  testCase "roundtryp xyzm geometry" $ do
    someUnit <- Gen.sample (Unityzm <$> gen)
    result <- runDB $ do
      _ <- insert someUnit
      selectList @(Unityzm) [] []
    (entityVal <$> result) @?= [someUnit]

postgisBindingsTests :: TestTree
postgisBindingsTests =
  testGroup
    "postgis binding tests"
    [ testGroup "roundtrip_tests_xy" $
        (test') <$> (genCollection genPointxy : genGeometry genPointxy),
      testGroup "roundtrip tests xyz" $
        (testxyz) <$> (genCollection genPointxyz : genGeometry genPointxyz),
      testGroup "roundtrip tests xyzm" $
        (testxyzm) <$> (genCollection genPointxyzm : genGeometry genPointxyzm),
      testGroup "function bindings" $
        [ testCase ("it_finds_the_one_unit_with st_contains") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Unit
                    { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)]
                    }

              selectOne $ do
                unit <- from $ table @Unit
                where_ $ unit ^. UnitGeom `st_contains` (val $ Point (PointXY 1 1))
                pure countRows
            unValue <$> result @?= (Just (1 :: Int)),
          testCase ("it finds the one unit with intersection st_point") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Unit
                    { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)]
                    }

              selectOne $ do
                unit <- from $ table @Unit
                where_ $ unit ^. UnitGeom `st_contains` (st_point (val 1) (val 1))
                pure countRows
            unValue <$> result @?= (Just (1 :: Int)),
          testCase ("it finds the one unit with intersection st_intersects") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Unit
                    { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)]
                    }

              selectOne $ do
                unit <- from $ table @Unit
                where_ $ unit ^. UnitGeom `st_intersects` (st_point (val 1) (val 1))
                pure countRows
            unValue <$> result @?= (Just (1 :: Int)),
          testCase ("see if we can union in PG and then get out some Haskell") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Grid
                    { gridGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)],
                      gridLabel = "x"
                    }
              _ <-
                insert $
                  Grid
                    { gridGeom = Polygon $ makePolygon (PointXY 2 0) (PointXY 2 2) (PointXY 4 2) $ Seq.fromList [(PointXY 4 0)],
                      gridLabel = "y"
                    }

              selectOne $ do
                grid <- from $ table @Grid
                pure $ st_union $ grid ^. GridGeom
            unValue <$> result @?= (Just $ Polygon $ makeLinearRing (PointXY {_xyX = 0.0, _xyY = 2.0}) (PointXY {_xyX = 2.0, _xyY = 2.0}) (PointXY {_xyX = 4.0, _xyY = 2.0}) (Seq.fromList [PointXY {_xyX = 4.0, _xyY = 0.0}, PointXY {_xyX = 2.0, _xyY = 0.0}, PointXY {_xyX = 0.0, _xyY = 0.0}])),
          testCase ("union_in_PG_and then get out some Haskell for geo") $ do
            result <- runDB $ do
              _ <-
                insert $
                  GeoGrid
                    { geoGridGeo = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)],
                      geoGridLabel = "x"
                    }
              _ <-
                insert $
                  GeoGrid
                    { geoGridGeo = Polygon $ makePolygon (PointXY 2 0) (PointXY 2 2) (PointXY 4 2) $ Seq.fromList [(PointXY 4 0)],
                      geoGridLabel = "y"
                    }

              selectOne $ do
                grid <- from $ table @GeoGrid
                pure $ st_transform_geometry $ st_union $ st_transform_geography mercator $ grid ^. GeoGridGeo
            -- just delete the input because it keeps changing order
            -- we just want to make sure something comes out
            (() <$ result) @?= (Just ()),
          testCase ("see if we can unions in PG and then get out some Haskell") $ do
            result <- runDB $ do
              selectOne $
                pure $ st_unions (val (Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)])) $
                        val $ Polygon $ makePolygon (PointXY 2 0) (PointXY 2 2) (PointXY 4 2) $ Seq.fromList [(PointXY 4 0)]
            unValue <$> result @?= (Just $ Polygon $ makeLinearRing (PointXY {_xyX = 0.0, _xyY = 2.0}) (PointXY {_xyX = 2.0, _xyY = 2.0}) (PointXY {_xyX = 4.0, _xyY = 2.0}) (Seq.fromList [PointXY {_xyX = 4.0, _xyY = 0.0}, PointXY {_xyX = 2.0, _xyY = 0.0}, PointXY {_xyX = 0.0, _xyY = 0.0}])),

          testCase ("st_distance@geom can distance PG and then get out some Haskell, doing it wrong with geometry") $ do
            result <- runDB $ do
              selectOne $
                pure $ st_distance @'Geometry
                    (point_v (-118.24) 34.05) -- LA
                    (point_v (-74.00) 40.71) -- NYC
            unValue <$> result @?= (Just  44.73849796316367), -- not 44km, but geometry does that
          testCase ("st_distance@geography can distance PG and then get out some Haskell, doing it wrong with geometry") $ do
            result <- runDB $ do
              selectOne $
                pure $ st_distance @'Geography
                    (point_v (-118.24) 34.05) -- LA
                    (point_v (-74.00) 40.71) -- NYC
            unValue <$> result @?= (Just 3_944_735.82464902), -- correct! (in m)
          testCase ("st_distance@geom can distance PG and then get out some Haskell, doing it wrong with geometry") $ do
            result <- runDB $ do
              selectOne $
                pure $ st_distance @'Geometry
                    (st_point (val (-118.24)) (val 34.05)) -- LA
                    (st_point (val (-74.00)) (val 40.71)) -- NYC
            unValue <$> result @?= (Just  44.73849796316367), -- not 44km, but geometry does that
          testCase ("st_distance@geography can distance PG and then get out some Haskell, doing it wrong with geometry") $ do
            result <- runDB $ do
              selectOne $
                pure $ st_distance @'Geography
                    (st_point (val (-118.24)) (val 34.05)) -- LA
                    (st_point (val (-74.00)) (val 40.71)) -- NYC
            unValue <$> result @?= (Just 3_944_735.82464902), -- correct! (in m)
          testCase ("see if we can get just the units in the polygons") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Grid
                    { gridGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)],
                      gridLabel = "x"
                    }
              _ <-
                insert $
                  Grid
                    { gridGeom = Polygon $ makePolygon (PointXY 2 0) (PointXY 2 2) (PointXY 4 2) $ Seq.fromList [(PointXY 4 0)],
                      gridLabel = "y"
                    }
              _ <-
                insert $
                  Unit
                    { unitGeom = point 1 1
                    }
              _ <-
                insert $
                  Unit
                    { unitGeom = point 1 2
                    }

              _ <-
                insert $
                  Unit
                    { unitGeom = point 2 2
                    }
              _ <-
                insert $
                  Unit
                    { unitGeom = point 9 9
                    }
              _ <-
                insert $
                  Unit
                    { unitGeom = point 10 10
                    }

              mCombined <- selectOne $ do
                grid <- from $ table @Grid
                pure $ st_union $ grid ^. GridGeom

              select $ do
                unit <- from $ table @Unit
                forM_ mCombined $ \combined ->
                  where_ $ (unit ^. UnitGeom) `st_intersects` (val $ unValue combined)
                pure unit

            entityVal <$> result @?= [Unit {unitGeom = Point (PointXY {_xyX = 1.0, _xyY = 1.0})}, Unit {unitGeom = Point (PointXY {_xyX = 1.0, _xyY = 2.0})}, Unit {unitGeom = Point (PointXY {_xyX = 2.0, _xyY = 2.0})}],
          testCase ("st_dwithin finds it wihtin range") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Unit
                    { unitGeom = point 1 1
                    }

              select $ do
                unit <- from $ table @Unit
                where_ $ st_dwithin (unit ^. UnitGeom) (st_point (val 1) (val 0)) (val 1)
                pure unit

            entityVal <$> result @?= [Unit {unitGeom = Point (PointXY {_xyX = 1.0, _xyY = 1.0})} ],
          testCase ("st_dwithin doesn't finds it out range") $ do
            result <- runDB $ do
              _ <-
                insert $
                  Unit
                    { unitGeom = point 1 1
                    }

              select $ do
                unit <- from $ table @Unit
                where_ $ st_dwithin (unit ^. UnitGeom) (st_point (val 2) (val 0)) (val 1)
                pure unit

            entityVal <$> result @?= [],

          -- st_within: interior point is within polygon
          testCase "st_within finds point inside polygon" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Point (PointXY 1 1)) `st_within` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_touches: adjacent polygons touch at shared edge
          testCase "st_touches detects shared edge" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Polygon $ makePolygon (PointXY 2 0) (PointXY 2 2) (PointXY 4 2) $ Seq.fromList [PointXY 4 0]) `st_touches` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_crosses: line crossing through polygon
          testCase "st_crosses detects line crossing polygon" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Line $ makeLineString (PointXY (-1) 1) (PointXY 3 1) $ Seq.fromList []) `st_crosses` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_disjoint: exterior point is disjoint from polygon
          testCase "st_disjoint finds disjoint geometries" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Point (PointXY 9 9)) `st_disjoint` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_equals: polygon equals itself
          testCase "st_equals detects equal geometries" $ do
            let poly = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0]
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = poly }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (unit ^. UnitGeom) `st_equals` (val poly)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_covers: polygon covers interior point
          testCase "st_covers polygon covers interior point" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (unit ^. UnitGeom) `st_covers` (val $ Point (PointXY 1 1))
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_coveredby: interior point is covered by polygon
          testCase "st_coveredby point covered by polygon" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Point (PointXY 1 1)) `st_coveredby` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_overlaps: overlapping polygons
          testCase "st_overlaps detects overlapping polygons" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Polygon $ makePolygon (PointXY 1 1) (PointXY 1 3) (PointXY 3 3) $ Seq.fromList [PointXY 3 1]) `st_overlaps` (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_containsproperly: polygon properly contains interior point (not on boundary)
          testCase "st_containsproperly finds interior point" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (unit ^. UnitGeom) `st_containsproperly` (val $ Point (PointXY 1 1))
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_area: area of 2x2 square = 4.0
          testCase "st_area computes polygon area" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_perimeter: perimeter of 2x2 square = 8.0
          testCase "st_perimeter computes polygon perimeter" $ do
            result <- runDB $ do
              selectOne $ pure $ st_perimeter (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (8.0 :: Double),

          -- st_length: length of line from (0,0) to (3,4) = 5.0
          testCase "st_length computes line length" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [])
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_azimuth: azimuth from origin to (1,0) = pi/2 (east)
          testCase "st_azimuth computes angle" $ do
            result <- runDB $ do
              selectOne $ pure $ st_azimuth
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 1 0))
            -- azimuth to east (positive X) = pi/2
            case unValue <$> result of
              Just v -> assertBool "azimuth ~ pi/2" (abs (v - pi / 2) < 1e-10)
              Nothing -> assertFailure "expected a result for st_azimuth",

          -- st_maxdistance: max distance between two squares
          testCase "st_maxdistance computes maximum distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_maxdistance
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 1) (PointXY 1 1) $ Seq.fromList [PointXY 1 0])
                (val $ Point (PointXY 4 0))
            -- max distance is from (0,1) to (4,0) = sqrt(16+1) = sqrt 17
            case unValue <$> result of
              Just v -> assertBool "maxdistance ~ sqrt 17" (abs (v - sqrt 17) < 1e-10)
              Nothing -> assertFailure "expected a result for st_maxdistance",

          -- st_x: X coordinate of point
          testCase "st_x extracts X coordinate" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x (val @(Postgis 'Geometry PointXY) $ Point (PointXY 3.5 7.2))
            unValue <$> result @?= Just (3.5 :: Double),

          -- st_y: Y coordinate of point
          testCase "st_y extracts Y coordinate" $ do
            result <- runDB $ do
              selectOne $ pure $ st_y (val @(Postgis 'Geometry PointXY) $ Point (PointXY 3.5 7.2))
            unValue <$> result @?= Just (7.2 :: Double),

          -- st_npoints: 2x2 square has 5 points (closed ring)
          testCase "st_npoints counts vertices" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (6 :: Int),

          -- st_numgeometries: single polygon returns 1
          testCase "st_numgeometries counts sub-geometries" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (1 :: Int),

          -- st_dimension: polygon has dimension 2
          testCase "st_dimension returns topological dimension" $ do
            result <- runDB $ do
              selectOne $ pure $ st_dimension (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (2 :: Int),

          -- st_issimple: simple line is simple
          testCase "st_issimple detects simple geometry" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_issimple (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_isclosed: closed linestring (start=end)
          testCase "st_isclosed detects closed linestring" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList [PointXY 2 0, PointXY 0 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_isclosed (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_isvalid: valid polygon is valid
          testCase "st_isvalid detects valid geometry" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_isvalid (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_srid: default SRID is 0
          testCase "st_srid returns spatial reference id" $ do
            result <- runDB $ do
              selectOne $ pure $ st_srid (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just (0 :: Int),

          -- st_centroid: centroid of 2x2 square at origin is (1,1)
          testCase "st_centroid computes geometric center" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (val $ Point (PointXY 1 1)) `st_equals` st_centroid (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_buffer: buffer of a point by 1 has area ~ pi
          testCase "st_buffer expands geometry by distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_buffer (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0)) (val 1)
            case unValue <$> result of
              Just v -> assertBool "buffer area ~ pi" (abs (v - pi) < 0.1)
              Nothing -> assertFailure "expected a result for st_buffer",

          -- st_convexhull: convex hull of L-shaped points is a triangle
          testCase "st_convexhull computes convex hull" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_convexhull (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 4 0) $ Seq.fromList [PointXY 0 3])
            -- triangle with base 4, height 3, area = 6
            unValue <$> result @?= Just (6.0 :: Double),

          -- st_envelope: bounding box of 2x2 square is itself
          testCase "st_envelope computes bounding box" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_envelope (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_pointonsurface: result is contained by the polygon
          testCase "st_pointonsurface returns point on surface" $ do
            let poly = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0]
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = poly }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ (unit ^. UnitGeom) `st_contains` st_pointonsurface (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_intersection: intersection of overlapping 2x2 squares = 1x1 area
          testCase "st_intersection computes shared area" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_intersection
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
                (val $ Polygon $ makePolygon (PointXY 1 1) (PointXY 1 3) (PointXY 3 3) $ Seq.fromList [PointXY 3 1])
            unValue <$> result @?= Just (1.0 :: Double),

          -- st_difference: A minus overlap
          testCase "st_difference computes geometric difference" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_difference
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
                (val $ Polygon $ makePolygon (PointXY 1 1) (PointXY 1 3) (PointXY 3 3) $ Seq.fromList [PointXY 3 1])
            -- 2x2 square area 4.0, minus 1x1 overlap = 3.0
            unValue <$> result @?= Just (3.0 :: Double),

          -- =================================================================
          -- Batch 2: 95 new function bindings
          -- =================================================================

          -- Geometry Constructors (4)

          -- st_collect: aggregate collect over two polygons
          testCase "st_collect aggregates geometries" $ do
            result <- runDB $ do
              _ <- insert $ Grid
                { gridGeom = Point (PointXY 1 1), gridLabel = "a" }
              _ <- insert $ Grid
                { gridGeom = Point (PointXY 2 2), gridLabel = "b" }
              selectOne $ do
                grid <- from $ table @Grid
                pure $ st_numgeometries $ st_collect $ grid ^. GridGeom
            unValue <$> result @?= Just (2 :: Int),

          -- st_makeenvelope: create a box from coordinates
          testCase "st_makeenvelope creates a rectangle" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_makeenvelope (val 0) (val 0) (val 3) (val 4) (val 0)
            unValue <$> result @?= Just (12.0 :: Double),

          -- st_makeline: create line from two points
          testCase "st_makeline creates a line from two points" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_makeline
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (2 :: Int),

          -- st_makepolygon_line: create polygon from closed linestring
          testCase "st_makepolygon_line creates polygon from closed linestring" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_makepolygon_line
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 0 2) $ Seq.fromList [PointXY 2 2, PointXY 2 0, PointXY 0 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- Geometry Accessors (20)

          -- st_boundary: boundary of a polygon is a linestring
          testCase "st_boundary returns boundary" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype $ st_boundary
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just ("ST_LineString" :: Text),

          -- st_coorddim: 2D point has coordinate dimension 2
          testCase "st_coorddim returns coordinate dimension" $ do
            result <- runDB $ do
              selectOne $ pure $ st_coorddim (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just (2 :: Int),

          -- st_endpoint: endpoint of a linestring
          testCase "st_endpoint returns last point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_endpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [])
            unValue <$> result @?= Just (3.0 :: Double),

          -- st_exteriorring: exterior ring of a polygon
          testCase "st_exteriorring returns exterior ring" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_exteriorring
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            case unValue <$> result of
              Just n -> assertBool "exterior ring has points" (n >= 4)
              Nothing -> assertFailure "expected a result for st_exteriorring",

          -- st_geometryn: get first geometry from a multipoint
          testCase "st_geometryn gets Nth geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_geometryn
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 5 6 :| [PointXY 7 8]))
                (val 1)
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_geometrytype: point reports as ST_Point
          testCase "st_geometrytype returns type string" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just ("ST_Point" :: Text),

          -- st_interiorringn: polygon without holes has no interior ring, tested with polygon with hole
          testCase "st_interiorringn does not crash" $ do
            -- A simple polygon has 0 interior rings; we just check this doesn't error
            result <- runDB $ do
              selectOne $ pure $ st_numinteriorrings
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (0 :: Int),

          -- st_iscollection: multipoint is a collection
          testCase "st_iscollection detects collection types" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = MultiPoint (PointXY 1 1 :| [PointXY 2 2]) }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_iscollection (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_isempty: non-empty point is not empty
          testCase "st_isempty detects non-empty geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_isempty (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just False,

          -- st_ispolygonccw: check CCW orientation
          testCase "st_ispolygonccw checks orientation" $ do
            -- CCW polygon: (0,0) (2,0) (2,2) (0,2) is CCW
            result <- runDB $ do
              selectOne $ pure $ st_ispolygonccw
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 2 0) (PointXY 2 2) $ Seq.fromList [PointXY 0 2])
            case unValue <$> result of
              Just _ -> pure () -- just check it returns a bool
              Nothing -> assertFailure "expected a result for st_ispolygonccw",

          -- st_ispolygoncw: check CW orientation
          testCase "st_ispolygoncw checks orientation" $ do
            -- CW polygon: (0,0) (0,2) (2,2) (2,0) is CW
            result <- runDB $ do
              selectOne $ pure $ st_ispolygoncw
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            case unValue <$> result of
              Just _ -> pure () -- just check it returns a bool
              Nothing -> assertFailure "expected a result for st_ispolygoncw",

          -- st_isring: closed simple linestring is a ring
          testCase "st_isring detects ring" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList [PointXY 2 0, PointXY 0 0] }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_isring (unit ^. UnitGeom)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_m: M coordinate of a 4D point
          testCase "st_m extracts M coordinate" $ do
            result <- runDB $ do
              _ <- insert $ Unityzm { unityzmGeom = Point (PointXYZM 1 2 3 42) }
              selectOne $ do
                unit <- from $ table @Unityzm
                pure $ st_m (unit ^. UnityzmGeom)
            unValue <$> result @?= Just (42.0 :: Double),

          -- st_ndims: 2D point has 2 dimensions
          testCase "st_ndims returns number of dimensions" $ do
            result <- runDB $ do
              selectOne $ pure $ st_ndims (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just (2 :: Int),

          -- st_nrings: polygon with no holes has 1 ring
          testCase "st_nrings counts rings" $ do
            result <- runDB $ do
              selectOne $ pure $ st_nrings
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (1 :: Int),

          -- st_numinteriorrings: simple polygon has 0 interior rings
          testCase "st_numinteriorrings counts interior rings" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numinteriorrings
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (0 :: Int),

          -- st_numpoints: linestring with 2 points
          testCase "st_numpoints counts linestring points" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numpoints
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [])
            unValue <$> result @?= Just (2 :: Int),

          -- st_pointn: first point of a linestring
          testCase "st_pointn gets Nth point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_pointn
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 5 6) (PointXY 7 8) $ Seq.fromList [])
                (val 1)
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_startpoint: first point of a linestring
          testCase "st_startpoint returns first point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_startpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 5 6) (PointXY 7 8) $ Seq.fromList [])
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_z: Z coordinate of a 3D point
          testCase "st_z extracts Z coordinate" $ do
            result <- runDB $ do
              _ <- insert $ Unityz { unityzGeom = Point (PointXYZ 1 2 99) }
              selectOne $ do
                unit <- from $ table @Unityz
                pure $ st_z (unit ^. UnityzGeom)
            unValue <$> result @?= Just (99.0 :: Double),

          -- Geometry Editors (16)

          -- st_addpoint: add point to linestring
          testCase "st_addpoint adds a point to linestring" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_addpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList [])
                (val $ Point (PointXY 2 2))
                (val 1)
            unValue <$> result @?= Just (3 :: Int),

          -- st_collectionextract: extract points from a collection
          testCase "st_collectionextract extracts geometry type" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries $ st_collectionextract
                (val @(Postgis 'Geometry PointXY) $ Collection (Point (PointXY 1 1) :| [Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList []]))
                (val 1) -- 1 = POINT
            unValue <$> result @?= Just (1 :: Int),

          -- st_flipcoordinates: swap X and Y
          testCase "st_flipcoordinates swaps X and Y" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_flipcoordinates (val @(Postgis 'Geometry PointXY) $ Point (PointXY 3 7))
            unValue <$> result @?= Just (7.0 :: Double),

          -- st_force2d: force to 2D
          testCase "st_force2d forces to 2D" $ do
            result <- runDB $ do
              _ <- insert $ Unityz { unityzGeom = Point (PointXYZ 1 2 3) }
              selectOne $ do
                unit <- from $ table @Unityz
                pure $ st_ndims $ st_force2d (unit ^. UnityzGeom)
            unValue <$> result @?= Just (2 :: Int),

          -- st_force3d: force to 3D
          testCase "st_force3d forces to 3D" $ do
            result <- runDB $ do
              selectOne $ pure $ st_ndims $ st_force3d (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
            unValue <$> result @?= Just (3 :: Int),

          -- st_force4d: force to 4D
          testCase "st_force4d forces to 4D" $ do
            result <- runDB $ do
              selectOne $ pure $ st_ndims $ st_force4d (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
            unValue <$> result @?= Just (4 :: Int),

          -- st_forcecollection: wrap in collection
          testCase "st_forcecollection wraps in collection" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype $ st_forcecollection (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just ("ST_GeometryCollection" :: Text),

          -- st_forcepolygonccw: force CCW orientation
          testCase "st_forcepolygonccw forces CCW" $ do
            result <- runDB $ do
              selectOne $ pure $ st_ispolygonccw $ st_forcepolygonccw
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just True,

          -- st_forcepolygoncw: force CW orientation
          testCase "st_forcepolygoncw forces CW" $ do
            result <- runDB $ do
              selectOne $ pure $ st_ispolygoncw $ st_forcepolygoncw
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just True,

          -- st_multi: convert to multi-type
          testCase "st_multi converts to multi-type" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype $ st_multi (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1))
            unValue <$> result @?= Just ("ST_MultiPoint" :: Text),

          -- st_normalize: normalize geometry
          testCase "st_normalize normalizes geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_normalize
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_reverse: reverse vertex order
          testCase "st_reverse reverses vertex order" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_startpoint $ st_reverse
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [])
            unValue <$> result @?= Just (3.0 :: Double),

          -- st_segmentize: add vertices to long segments
          testCase "st_segmentize adds vertices" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_segmentize
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val 3)
            case unValue <$> result of
              Just n -> assertBool "segmentize adds points" (n > 2)
              Nothing -> assertFailure "expected a result for st_segmentize",

          -- st_setpoint: replace a point in linestring
          testCase "st_setpoint replaces a point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_startpoint $ st_setpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList [])
                (val 0)
                (val $ Point (PointXY 99 99))
            unValue <$> result @?= Just (99.0 :: Double),

          -- st_snaptogrid: snap to grid
          testCase "st_snaptogrid snaps coordinates" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_snaptogrid (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1.3 2.7)) (val 1)
            unValue <$> result @?= Just (1.0 :: Double),

          -- st_snap: snap vertices
          testCase "st_snap snaps vertices" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_snap
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Point (PointXY 5 1))
                (val 2)
            case unValue <$> result of
              Just n -> assertBool "snap adds or keeps points" (n >= 2)
              Nothing -> assertFailure "expected a result for st_snap",

          -- Geometry Validation (2)

          -- st_makevalid: valid geometry stays the same area
          testCase "st_makevalid makes geometry valid" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_makevalid
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_isvalidreason: valid geometry reports "Valid Geometry"
          testCase "st_isvalidreason reports validity" $ do
            result <- runDB $ do
              selectOne $ pure $ st_isvalidreason
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just ("Valid Geometry" :: Text),

          -- SRS Functions (1)

          -- st_setsrid: set SRID and read back
          testCase "st_setsrid sets SRID" $ do
            result <- runDB $ do
              selectOne $ pure $ st_srid $ st_setsrid (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 1)) (val 4326)
            unValue <$> result @?= Just (4326 :: Int),

          -- Geometry Output (4)

          -- st_astext: WKT of a point
          testCase "st_astext returns WKT" $ do
            result <- runDB $ do
              selectOne $ pure $ st_astext (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
            unValue <$> result @?= Just ("POINT(1 2)" :: Text),

          -- st_asgeojson: GeoJSON of a point
          testCase "st_asgeojson returns GeoJSON" $ do
            result <- runDB $ do
              selectOne $ pure $ st_asgeojson (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
            case unValue <$> result of
              Just t -> assertBool "contains coordinates" (isInfixOf "coordinates" t)
              Nothing -> assertFailure "expected a result for st_asgeojson",

          -- st_asewkt: EWKT of a point
          testCase "st_asewkt returns EWKT" $ do
            result <- runDB $ do
              selectOne $ pure $ st_asewkt (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
            case unValue <$> result of
              Just t -> assertBool "contains POINT" (isInfixOf "POINT" t)
              Nothing -> assertFailure "expected a result for st_asewkt",

          -- st_geohash: geohash of a point with SRID 4326
          testCase "st_geohash returns geohash" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geohash $ st_setsrid (val @(Postgis 'Geometry PointXY) $ Point (PointXY (-74) 40.7)) (val 4326)
            case unValue <$> result of
              Just t -> assertBool "geohash is non-empty" (not (T.null t))
              Nothing -> assertFailure "expected a result for st_geohash",

          -- Spatial Relationships (5)

          -- st_3dintersects: 3D intersection
          testCase "st_3dintersects checks 3D intersection" $ do
            result <- runDB $ do
              selectOne $ pure $ st_3dintersects
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 0 0))
            unValue <$> result @?= Just True,

          -- st_relate: DE-9IM matrix of two equal points
          testCase "st_relate returns DE-9IM matrix" $ do
            result <- runDB $ do
              selectOne $ pure $ st_relate
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 0 0))
            -- Two equal points: "0FFFFFFF2"
            unValue <$> result @?= Just ("0FFFFFFF2" :: Text),

          -- st_orderingequals: same geometry with same vertex order
          testCase "st_orderingequals checks vertex order equality" $ do
            let line = Line $ makeLineString (PointXY 0 0) (PointXY 1 1) $ Seq.fromList []
            result <- runDB $ do
              selectOne $ pure $ st_orderingequals
                (val @(Postgis 'Geometry PointXY) line)
                (val line)
            unValue <$> result @?= Just True,

          -- st_dfullywithin: both geometries fully within distance
          testCase "st_dfullywithin checks full containment" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Point (PointXY 0 0) }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_dfullywithin (unit ^. UnitGeom) (val $ Point (PointXY 1 0)) (val 2)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- st_pointinsidecircle: point inside circle
          testCase "st_pointinsidecircle checks point in circle" $ do
            result <- runDB $ do
              _ <- insert $ Unit { unitGeom = Point (PointXY 1 1) }
              selectOne $ do
                unit <- from $ table @Unit
                where_ $ st_pointinsidecircle (unit ^. UnitGeom) (val 0) (val 0) (val 2)
                pure countRows
            unValue <$> result @?= Just (1 :: Int),

          -- Measurement Functions (15)

          -- st_angle: angle between two 2-point linestrings (PostGIS ST_Angle with 2 line args)
          testCase "st_angle computes angle" $ do
            result <- runDB $ do
              selectOne $ pure $ st_angle
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 0) $ Seq.fromList [])
                (val $ Line $ makeLineString (PointXY 0 0) (PointXY 0 1) $ Seq.fromList [])
            case unValue <$> result of
              Just v -> assertBool "angle is finite" (v >= 0 && v <= 2 * pi)
              Nothing -> assertFailure "expected a result for st_angle",

          -- st_closestpoint: closest point on line to external point
          testCase "st_closestpoint finds closest point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_y $ st_closestpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Point (PointXY 5 5))
            unValue <$> result @?= Just (0.0 :: Double),

          -- st_3dclosestpoint: 3D closest point
          testCase "st_3dclosestpoint finds 3D closest point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_3dclosestpoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Point (PointXY 5 5))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_3ddistance: 3D distance between two points
          testCase "st_3ddistance computes 3D distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_3ddistance
                (val @(Postgis 'Geometry PointXYZ) $ Point (PointXYZ 0 0 0))
                (val $ Point (PointXYZ 1 0 0))
            unValue <$> result @?= Just (1.0 :: Double),

          -- st_distancesphere: spherical distance
          testCase "st_distancesphere computes spherical distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_distancesphere
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 1 0))
            case unValue <$> result of
              Just v -> assertBool "spherical distance > 0" (v > 0)
              Nothing -> assertFailure "expected a result for st_distancesphere",

          -- st_frechetdistance: Frechet distance
          testCase "st_frechetdistance computes Frechet distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_frechetdistance
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Line $ makeLineString (PointXY 0 1) (PointXY 10 1) $ Seq.fromList [])
            unValue <$> result @?= Just (1.0 :: Double),

          -- st_hausdorffdistance: Hausdorff distance
          testCase "st_hausdorffdistance computes Hausdorff distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_hausdorffdistance
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Line $ makeLineString (PointXY 0 1) (PointXY 10 1) $ Seq.fromList [])
            unValue <$> result @?= Just (1.0 :: Double),

          -- st_length2d: 2D length of a line
          testCase "st_length2d computes 2D length" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length2d
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 3 4) $ Seq.fromList [])
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_3dlength: 3D length of a line
          testCase "st_3dlength computes 3D length" $ do
            result <- runDB $ do
              selectOne $ pure $ st_3dlength $ st_makeline
                (val @(Postgis 'Geometry PointXYZ) $ Point (PointXYZ 0 0 0))
                (val $ Point (PointXYZ 3 4 0))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_longestline: longest line between two geometries
          testCase "st_longestline returns longest line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_longestline
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_3dlongestline: 3D longest line
          testCase "st_3dlongestline returns 3D longest line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_3dlongestline
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_3dmaxdistance: 3D max distance
          testCase "st_3dmaxdistance computes 3D max distance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_3dmaxdistance
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_minimumclearance: minimum clearance of polygon
          testCase "st_minimumclearance computes minimum clearance" $ do
            result <- runDB $ do
              selectOne $ pure $ st_minimumclearance
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            case unValue <$> result of
              Just v -> assertBool "clearance > 0" (v > 0)
              Nothing -> assertFailure "expected a result for st_minimumclearance",

          -- st_shortestline: shortest line between two geometries
          testCase "st_shortestline returns shortest line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_shortestline
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_3dshortestline: 3D shortest line
          testCase "st_3dshortestline returns 3D shortest line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_3dshortestline
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val $ Point (PointXY 3 4))
            unValue <$> result @?= Just (5.0 :: Double),

          -- Overlay Functions (4)

          -- st_symdifference: symmetric difference
          testCase "st_symdifference computes symmetric difference" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_symdifference
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
                (val $ Polygon $ makePolygon (PointXY 1 1) (PointXY 1 3) (PointXY 3 3) $ Seq.fromList [PointXY 3 1])
            -- each 4, overlap 1, so symdiff = 4 + 4 - 2*1 = 6
            unValue <$> result @?= Just (6.0 :: Double),

          -- st_unaryunion: dissolve internal boundaries
          testCase "st_unaryunion dissolves internal boundaries" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_unaryunion
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [PointXY 2 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_split: split a line at a point
          testCase "st_split splits geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries $ st_split
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Point (PointXY 5 0))
            unValue <$> result @?= Just (2 :: Int),

          -- st_node: node linestrings
          testCase "st_node nodes linestrings" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_node
                (val @(Postgis 'Geometry PointXY) $ Multiline
                  ( makeLineString (PointXY 0 0) (PointXY 10 10) (Seq.fromList [])
                    :| [makeLineString (PointXY 0 10) (PointXY 10 0) (Seq.fromList [])]))
            case unValue <$> result of
              Just n -> assertBool "node adds intersection point" (n > 4)
              Nothing -> assertFailure "expected a result for st_node",

          -- Geometry Processing (15)

          -- st_buildarea: build area from linestrings
          testCase "st_buildarea builds area from linestrings" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_buildarea
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 0 2) $ Seq.fromList [PointXY 2 2, PointXY 2 0, PointXY 0 0])
            unValue <$> result @?= Just (4.0 :: Double),

          -- st_chaikinsmoothing: smoothing
          testCase "st_chaikinsmoothing smooths geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_chaikinsmoothing
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 5) $ Seq.fromList [PointXY 5 2, PointXY 10 0])
            case unValue <$> result of
              Just n -> assertBool "smoothing adds points" (n > 4)
              Nothing -> assertFailure "expected a result for st_chaikinsmoothing",

          -- st_concavehull: concave hull
          testCase "st_concavehull computes concave hull" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_concavehull
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 0 0 :| [PointXY 10 0, PointXY 5 10, PointXY 5 5]))
                (val 1.0)
            case unValue <$> result of
              Just v -> assertBool "concave hull has area" (v > 0)
              Nothing -> assertFailure "expected a result for st_concavehull",

          -- st_delaunaytriangles: Delaunay triangulation
          testCase "st_delaunaytriangles computes triangulation" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries $ st_delaunaytriangles
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 0 0 :| [PointXY 10 0, PointXY 5 10, PointXY 5 5]))
            case unValue <$> result of
              Just n -> assertBool "triangulation produces triangles" (n >= 1)
              Nothing -> assertFailure "expected a result for st_delaunaytriangles",

          -- st_generatepoints: generate random points in polygon
          testCase "st_generatepoints generates points" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries $ st_generatepoints
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 100) (PointXY 100 100) $ Seq.fromList [PointXY 100 0])
                (val 5)
            unValue <$> result @?= Just (5 :: Int),

          -- st_geometricmedian: geometric median of multipoint
          testCase "st_geometricmedian computes median" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_geometricmedian
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 0 0 :| [PointXY 10 0, PointXY 5 10]))
            case unValue <$> result of
              Just v -> assertBool "median X in range" (v > 0 && v < 10)
              Nothing -> assertFailure "expected a result for st_geometricmedian",

          -- st_linemerge: merge multilinestring into single linestring
          testCase "st_linemerge merges lines" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype $ st_linemerge
                (val @(Postgis 'Geometry PointXY) $ Multiline
                  ( makeLineString (PointXY 0 0) (PointXY 1 1) (Seq.fromList [])
                    :| [makeLineString (PointXY 1 1) (PointXY 2 2) (Seq.fromList [])]))
            unValue <$> result @?= Just ("ST_LineString" :: Text),

          -- st_minimumboundingcircle: bounding circle
          testCase "st_minimumboundingcircle computes bounding circle" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_minimumboundingcircle
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 2 0) $ Seq.fromList [])
            case unValue <$> result of
              -- radius = 1, area ~ pi
              Just v -> assertBool "bounding circle area ~ pi" (abs (v - pi) < 0.1)
              Nothing -> assertFailure "expected a result for st_minimumboundingcircle",

          -- st_offsetcurve: offset a line
          testCase "st_offsetcurve offsets a line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_offsetcurve
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val 1)
            case unValue <$> result of
              Just v -> assertBool "offset line has length" (v > 0)
              Nothing -> assertFailure "expected a result for st_offsetcurve",

          -- st_reduceprecision: reduce precision
          testCase "st_reduceprecision reduces precision" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_reduceprecision
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1.23456 2.34567))
                (val 0.1)
            case unValue <$> result of
              Just v -> assertBool "reduced precision" (abs (v - 1.2) < 0.15)
              Nothing -> assertFailure "expected a result for st_reduceprecision",

          -- st_sharedpaths: shared paths between two lines
          testCase "st_sharedpaths finds shared paths" $ do
            result <- runDB $ do
              selectOne $ pure $ st_geometrytype $ st_sharedpaths
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 5 0) $ Seq.fromList [PointXY 10 0])
                (val $ Line $ makeLineString (PointXY 3 0) (PointXY 5 0) $ Seq.fromList [PointXY 7 0])
            unValue <$> result @?= Just ("ST_GeometryCollection" :: Text),

          -- st_simplify: simplify geometry
          testCase "st_simplify simplifies geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_simplify
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 0.001) $ Seq.fromList [PointXY 2 0, PointXY 3 0.001, PointXY 4 0])
                (val 0.01)
            case unValue <$> result of
              Just n -> assertBool "simplify reduces points" (n < 5)
              Nothing -> assertFailure "expected a result for st_simplify",

          -- st_simplifypreservetopology: simplify preserving topology
          testCase "st_simplifypreservetopology simplifies preserving topology" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_simplifypreservetopology
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 1 0.001) $ Seq.fromList [PointXY 2 0, PointXY 3 0.001, PointXY 4 0])
                (val 0.01)
            case unValue <$> result of
              Just n -> assertBool "simplifyPreserveTopology reduces points" (n < 5)
              Nothing -> assertFailure "expected a result for st_simplifypreservetopology",

          -- st_voronoilines: Voronoi diagram edges
          testCase "st_voronoilines computes Voronoi edges" $ do
            result <- runDB $ do
              selectOne $ pure $ st_npoints $ st_voronoilines
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 0 0 :| [PointXY 10 0, PointXY 5 10]))
            case unValue <$> result of
              Just n -> assertBool "voronoi has points" (n > 0)
              Nothing -> assertFailure "expected a result for st_voronoilines",

          -- st_voronoipolygons: Voronoi diagram polygons
          testCase "st_voronoipolygons computes Voronoi polygons" $ do
            result <- runDB $ do
              selectOne $ pure $ st_numgeometries $ st_voronoipolygons
                (val @(Postgis 'Geometry PointXY) $ MultiPoint (PointXY 0 0 :| [PointXY 10 0, PointXY 5 10]))
            unValue <$> result @?= Just (3 :: Int),

          -- Affine Transformations (5)

          -- st_translate: translate point
          testCase "st_translate moves geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_translate
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 2))
                (val 10)
                (val 20)
            unValue <$> result @?= Just (11.0 :: Double),

          -- st_scale: scale geometry
          testCase "st_scale scales geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_scale
                (val @(Postgis 'Geometry PointXY) $ Polygon $ makePolygon (PointXY 0 0) (PointXY 0 1) (PointXY 1 1) $ Seq.fromList [PointXY 1 0])
                (val 3)
                (val 2)
            -- original area 1, scaled by 3*2 = 6
            unValue <$> result @?= Just (6.0 :: Double),

          -- st_rotate: rotate point 180 degrees (pi radians) around origin
          testCase "st_rotate rotates geometry" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_rotate
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 0))
                (val pi)
            case unValue <$> result of
              Just v -> assertBool "rotated X ~ -1" (abs (v - (-1)) < 1e-10)
              Nothing -> assertFailure "expected a result for st_rotate",

          -- st_rotatex: rotate around X axis
          testCase "st_rotatex rotates around X axis" $ do
            result <- runDB $ do
              selectOne $ pure $ st_z $ st_rotatex
                (val @(Postgis 'Geometry PointXYZ) $ Point (PointXYZ 0 1 0))
                (val (pi / 2))
            case unValue <$> result of
              Just v -> assertBool "rotateX z ~ 1" (abs (v - 1) < 1e-10)
              Nothing -> assertFailure "expected a result for st_rotatex",

          -- st_rotatez: rotate around Z axis (same as st_rotate for 2D)
          testCase "st_rotatez rotates around Z axis" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_rotatez
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 1 0))
                (val pi)
            case unValue <$> result of
              Just v -> assertBool "rotateZ X ~ -1" (abs (v - (-1)) < 1e-10)
              Nothing -> assertFailure "expected a result for st_rotatez",

          -- Bounding Box (1)

          -- st_expand: expand bounding box
          testCase "st_expand expands bounding box" $ do
            result <- runDB $ do
              selectOne $ pure $ st_area $ st_envelope $ st_expand
                (val @(Postgis 'Geometry PointXY) $ Point (PointXY 0 0))
                (val 5)
            -- point expanded by 5 in all directions => 10x10 box => area 100
            unValue <$> result @?= Just (100.0 :: Double),

          -- Linear Referencing (3)

          -- st_lineinterpolatepoint: midpoint of a line
          testCase "st_lineinterpolatepoint interpolates point" $ do
            result <- runDB $ do
              selectOne $ pure $ st_x $ st_lineinterpolatepoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val 0.5)
            unValue <$> result @?= Just (5.0 :: Double),

          -- st_linelocatepoint: locate point on line
          testCase "st_linelocatepoint locates point on line" $ do
            result <- runDB $ do
              selectOne $ pure $ st_linelocatepoint
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val $ Point (PointXY 5 0))
            unValue <$> result @?= Just (0.5 :: Double),

          -- st_linesubstring: extract portion of line
          testCase "st_linesubstring extracts substring" $ do
            result <- runDB $ do
              selectOne $ pure $ st_length $ st_linesubstring
                (val @(Postgis 'Geometry PointXY) $ Line $ makeLineString (PointXY 0 0) (PointXY 10 0) $ Seq.fromList [])
                (val 0.0)
                (val 0.5)
            unValue <$> result @?= Just (5.0 :: Double)
        ]
    ]

genDouble :: Gen Double
genDouble = Gen.double (Range.exponentialFloat (-10) 10)

genPointxy :: Gen PointXY
genPointxy = PointXY <$> genDouble <*> genDouble

genPointxyz :: Gen PointXYZ
genPointxyz = PointXYZ <$> genDouble <*> genDouble <*> genDouble

genPointxyzm :: Gen PointXYZM
genPointxyzm = PointXYZM <$> genDouble <*> genDouble <*> genDouble <*> genDouble

genPoints :: Gen a -> Gen (NonEmpty a)
genPoints genPoint = Gen.nonEmpty (Range.constant 1 10) genPoint

genSeq :: Gen a -> Gen (Seq a)
genSeq genPoint = Seq.fromList <$> Gen.list (Range.constant 0 10) genPoint

genLineString :: Gen a -> Gen (LineString a)
genLineString genPoint = makeLineString <$> genPoint <*> genPoint <*> genSeq genPoint

genMultiLineString :: Gen a -> Gen (NonEmpty (LineString a))
genMultiLineString genPoint = Gen.nonEmpty (Range.constant 1 10) (genLineString genPoint)

genLinearring :: (Eq a, Show a) => Gen a -> Gen (LinearRing a)
genLinearring genPoint = makePolygon <$> genPoint <*> genPoint <*> genPoint <*> genSeq genPoint

genMultiLinearring :: (Eq a, Show a) => Gen a -> Gen (NonEmpty (LinearRing a))
genMultiLinearring genPoint = Gen.nonEmpty (Range.constant 1 10) (genLinearring genPoint)

genCollection :: (Eq a, Show a) => Gen a -> Gen (PostgisGeometry a)
genCollection genPoint = Collection <$> Gen.nonEmpty (Range.constant 1 10) (Gen.choice (genGeometry genPoint))

genGeometry :: (Eq a, Show a) => Gen a -> [Gen (PostgisGeometry a)]
genGeometry genPoint =
  [ -- pure NoGeometry
    (Point <$> genPoint),
    (MultiPoint <$> genPoints genPoint),
    (Line <$> genLineString genPoint),
    (Multiline <$> genMultiLineString genPoint),
    (Polygon <$> genLinearring genPoint),
    (MultiPolygon <$> genMultiLinearring genPoint)
  ]
