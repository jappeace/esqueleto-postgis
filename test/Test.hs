{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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

import Test.Tasty
import Test.Tasty.HUnit

import Data.List(sort)
import Database.Esqueleto.Postgis
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT)
import Control.Monad.Trans.Resource (MonadThrow, ResourceT, runResourceT)
import Database.Esqueleto
       ( SqlExpr
       , Value(..)
       , from
       , select
       , set
       , unValue
       , update
       , val
       , where_
       , (=.)
       , (^.)
       )
import Database.Persist
import Database.Persist.Postgresql
       ( ConnectionString
       , SqlPersistT
       , runMigration
       , runSqlConn
       , transactionUndo
       , withPostgresqlConn
       )
import Database.Persist.TH
       (mkMigrate, mkPersist, persistUpperCase, share, sqlSettings)
import Data.Geospatial (PointXY(..))
import Data.List.NonEmpty (NonEmpty)

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Unit
    geom       (PostgisGeometry PointXY)
    deriving Eq Show
|]

initializeDB
  :: ( MonadIO m, MonadLogger m, MonadThrow m)
  => SqlPersistT (ResourceT m) ()
initializeDB  = do
  runMigration migrateAll

runDB :: (forall m . (MonadIO m, MonadLogger m, MonadThrow m) => SqlPersistT (ResourceT m) a) -> IO a
runDB act
  = runStderrLoggingT
  . runResourceT
  . withPostgresqlConn connString
  . runSqlConn
  $ (initializeDB >> act >>= \ret -> transactionUndo >> return ret)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

test' :: Int -> TestTree
test' ix =
    testCase ("List comparison (different length)" <> show ix) $ do
        someUnit <- Gen.sample genUnit
        result <- runDB $ do
          insert someUnit
          selectList @(Unit) [] []
        (entityVal <$> result) @?= [someUnit]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
   (test') <$> [0..10]


genUnit :: Gen Unit
genUnit = Unit <$> genGeometry

genDouble = Gen.double (Range.exponentialFloat (-10) 10)

genPoint :: Gen PointXY
genPoint = PointXY <$> genDouble <*> genDouble

genPoints :: Gen (NonEmpty PointXY)
genPoints = Gen.nonEmpty (Range.constant 1 10) genPoint

genGeometry :: Gen (PostgisGeometry PointXY)
genGeometry = do
  Gen.choice [
    -- pure NoGeometry
     (Point <$> genPoint ),
     (MultiPoint <$> genPoints)
             ]
