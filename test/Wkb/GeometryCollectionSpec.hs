
{-# LANGUAGE OverloadedStrings #-}

module Wkb.GeometryCollectionSpec where

import           Test.Hspec      (Spec, describe)

import qualified Data.SpecHelper as SpecHelper

spec :: Spec
spec =
  describe "Test wkb geometry collection parsing" $
    SpecHelper.testRoundTripWkbGeometryParsing "geometry collection" SpecHelper.genGeometryCollection

