module Hydra.Pleco.ApiSpec (spec) where

import Hydra.Pleco.Api.Gen qualified as Gen

import Data.Aeson qualified as Aeson
import Data.OpenApi.Schema.Validation (validateToJSON)
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, tripping)

spec :: Spec
spec = do
  describe "Health" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        health <- forAll Gen.health
        tripping health Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        health <- forAll Gen.health
        validateToJSON health === []

  describe "Subscription" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        sub <- forAll Gen.subscription
        tripping sub Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        sub <- forAll Gen.subscription
        validateToJSON sub === []

  describe "JobsetEvent" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        ev <- forAll Gen.jobsetEvent
        tripping ev Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        ev <- forAll Gen.jobsetEvent
        validateToJSON ev === []

  describe "EventType" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        ty <- forAll Gen.eventType
        tripping ty Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        ty <- forAll Gen.eventType
        validateToJSON ty === []

  describe "Project" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        prj <- forAll Gen.project
        tripping prj Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        prj <- forAll Gen.project
        validateToJSON prj === []

  describe "ProjectId" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        id' <- forAll Gen.projectId
        tripping id' Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        id' <- forAll Gen.projectId
        validateToJSON id' === []

  describe "Jobset" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        jobset <- forAll Gen.jobset
        tripping jobset Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        jobset <- forAll Gen.jobset
        validateToJSON jobset === []

  describe "JobsetId" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        id' <- forAll Gen.jobsetId
        tripping id' Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        id' <- forAll Gen.jobsetId
        validateToJSON id' === []

  describe "JobsetName" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        name <- forAll Gen.jobsetName
        tripping name Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        name <- forAll Gen.jobsetName
        validateToJSON name === []

  describe "JobsetState" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        st <- forAll Gen.jobsetState
        tripping st Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        st <- forAll Gen.jobsetState
        validateToJSON st === []

  describe "JobsetType" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        jsType <- forAll Gen.jobsetType
        tripping jsType Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        jsType <- forAll Gen.jobsetType
        validateToJSON jsType === []

  describe "Eval" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        ev <- forAll Gen.eval
        tripping ev Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        ev <- forAll Gen.eval
        validateToJSON ev === []

  describe "EvalId" $ do
    it "round-trips through Aeson" $
      hedgehog $ do
        id' <- forAll Gen.evalId
        tripping id' Aeson.encode Aeson.eitherDecode

    it "conforms to its OpenApi schema" $
      hedgehog $ do
        id' <- forAll Gen.evalId
        validateToJSON id' === []
