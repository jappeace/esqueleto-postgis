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
import Data.Geospatial (PointXY (..), PointXYZ (..), PointXYZM (..))
import Data.LineString (LineString, makeLineString)
import Data.LinearRing (LinearRing, makeLinearRing)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text
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
import qualified Wkb.HexParsingSpec
import qualified Wkb.LineSpec
import qualified Wkb.PointSpec
import qualified Wkb.PolygonSpec

import qualified Wkt.BoxSpec
import qualified Wkt.GeometryCollectionSpec
import qualified Wkt.LineSpec
import qualified Wkt.PointSpec
import qualified Wkt.PolygonSpec

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistUpperCase|
  Grid
    geom (PostgisGeometry PointXY)
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
    testSpec "Wkb.HexParsingSpec" Wkb.HexParsingSpec.spec,
    testSpec "Wkb.LineSpec" Wkb.LineSpec.spec,
    testSpec "Wkb.PointSpec" Wkb.PointSpec.spec,
    testSpec "Wkb.PolygonSpec" Wkb.PolygonSpec.spec,

    testSpec "Wkt.BoxSpec" Wkt.BoxSpec.spec,
    testSpec "Wkt.GeometryCollectionSpec" Wkt.GeometryCollectionSpec.spec,
    testSpec "Wkt.LineSpec" Wkt.LineSpec.spec,
    testSpec "Wkt.PointSpec" Wkt.PointSpec.spec,
    testSpec "Wkt.PolygonSpec" Wkt.PolygonSpec.spec
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
    [ testGroup "roundtrip tests xy" $
        (test') <$> (genCollection genPointxy : genGeometry genPointxy),
      testGroup "roundtrip tests xyz" $
        (testxyz) <$> (genCollection genPointxyz : genGeometry genPointxyz),
      testGroup "roundtrip tests xyzm" $
        (testxyzm) <$> (genCollection genPointxyzm : genGeometry genPointxyzm),
      testGroup "function bindings" $
        [ testCase ("it finds the one unit with st_contains") $ do
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

            entityVal <$> result @?= [Unit {unitGeom = Point (PointXY {_xyX = 1.0, _xyY = 1.0})}, Unit {unitGeom = Point (PointXY {_xyX = 1.0, _xyY = 2.0})}, Unit {unitGeom = Point (PointXY {_xyX = 2.0, _xyY = 2.0})}]
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
