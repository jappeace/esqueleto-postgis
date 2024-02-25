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

import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger (..), runStderrLoggingT)
import Control.Monad.Trans.Resource (MonadThrow, ResourceT, runResourceT)
import Data.Geospatial (PointXY (..), PointXYZ (..), PointXYZM (..))
import Data.LineString (LineString, makeLineString)
import Data.LinearRing (LinearRing)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistUpperCase|
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
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

test' :: Gen (PostgisGeometry PointXY) -> TestTree
test' gen =
  testCase ("List comparison (different length)") $ do
    someUnit <- Gen.sample (Unit <$> gen)
    result <- runDB $ do
      _ <- insert someUnit
      selectList @(Unit) [] []
    (entityVal <$> result) @?= [someUnit]

testxyz :: Gen (PostgisGeometry PointXYZ) -> TestTree
testxyz gen =
  testCase ("List comparison (different length)") $ do
    someUnit <- Gen.sample (Unityz <$> gen)
    result <- runDB $ do
      _ <-  insert someUnit
      selectList @(Unityz) [] []
    (entityVal <$> result) @?= [someUnit]

testxyzm :: Gen (PostgisGeometry PointXYZM) -> TestTree
testxyzm gen =
  testCase ("List comparison (different length)") $ do
    someUnit <- Gen.sample (Unityzm <$> gen)
    result <- runDB $ do
      _ <- insert someUnit
      selectList @(Unityzm) [] []
    (entityVal <$> result) @?= [someUnit]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testGroup "roundtrip tests xy" $
        (test') <$> (genCollection genPointxy : genGeometry genPointxy),
      testGroup "roundtrip tests xyz" $
        (testxyz) <$> (genCollection genPointxyz : genGeometry genPointxyz),
      testGroup "roundtrip tests xyzm" $
        (testxyzm) <$> (genCollection genPointxyzm : genGeometry genPointxyzm),
      testGroup "function bindings" $
        [ testCase ("it finds the one unit with st_contains") $ do
            result <- runDB $ do
              _ <- insert $
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
              _ <- insert $
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
              _ <- insert $
                Unit
                  { unitGeom = Polygon $ makePolygon (PointXY 0 0) (PointXY 0 2) (PointXY 2 2) $ Seq.fromList [(PointXY 2 0)]
                  }

              selectOne $ do
                unit <- from $ table @Unit
                where_ $ unit ^. UnitGeom `st_intersects` (st_point (val 1) (val 1))
                pure countRows
            unValue <$> result @?= (Just (1 :: Int))
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
