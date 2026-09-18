module Hydra.Pleco.Server.DB.HydraSpec (spec) where

import Hydra.Pleco.Server.DB.Hydra
  ( HydraNotification (..),
    JobsetEvalId (..),
    JobsetId (..),
    parseHydraNotification,
    renderHydraNotification,
  )
import Hydra.Pleco.Server.Gen qualified as Gen

import Hedgehog (forAll)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, tripping)

spec :: Spec
spec = describe "parseHydraNotification" $ do
  it "round-trips a rendered notification" $
    hedgehog $ do
      notification <- forAll Gen.hydraNotification
      tripping 
        notification 
        (renderHydraNotification hydraTraceId)
        (uncurry parseHydraNotification)

  describe "eval_added" $ do
    it "parses a well-formed payload" $
      parseHydraNotification "eval_added" (hydraTraceId <> "\t123\t456")
        `shouldBe` Right (HydraEvalAdded (JobsetId 123) (JobsetEvalId 456))

    it "rejects malformed payloads" $
      for_ badThreeFieldPayloads $ \payload ->
        parseHydraNotification "eval_added" payload `shouldSatisfy` isLeft

  describe "eval_cached" $ do
    it "parses a well-formed payload" $
      parseHydraNotification "eval_cached" (hydraTraceId <> "\t123\t456")
        `shouldBe` Right (HydraEvalCached (JobsetId 123) (JobsetEvalId 456))

    it "rejects malformed payloads" $
      for_ badThreeFieldPayloads $ \payload ->
        parseHydraNotification "eval_cached" payload `shouldSatisfy` isLeft

  describe "eval_started" $ do
    it "parses a well-formed payload" $
      parseHydraNotification "eval_started" (hydraTraceId <> "\t123")
        `shouldBe` Right (HydraEvalStarted (JobsetId 123))

    it "rejects malformed payloads" $
      for_ badTwoFieldPayloads $ \payload ->
        parseHydraNotification "eval_started" payload `shouldSatisfy` isLeft

  describe "eval_failed" $ do
    it "parses a well-formed payload" $
      parseHydraNotification "eval_failed" (hydraTraceId <> "\t123")
        `shouldBe` Right (HydraEvalFailed (JobsetId 123))

    it "rejects malformed payloads" $
      for_ badTwoFieldPayloads $ \payload ->
        parseHydraNotification "eval_failed" payload `shouldSatisfy` isLeft

  it "rejects unknown channels" $
    parseHydraNotification "build_started" (hydraTraceId <> "\t123")
      `shouldSatisfy` isLeft

-- | Hydra sends a UUID.
hydraTraceId :: Text
hydraTraceId = "6a5b9d0e-0e13-4a4f-9e0c-2d5c6a1b7f8e"

-- | Ported from Hydra's @Hydra\/Event\/EvalAdded.t@.
badThreeFieldPayloads :: [Text]
badThreeFieldPayloads =
  [ "", -- An empty payload
    hydraTraceId, -- one field
    hydraTraceId <> "\t123", -- two fields
    hydraTraceId <> "\t123\t456\t789", -- four fields
    hydraTraceId <> "\tabc\t456", -- a non-integer jobset id
    hydraTraceId <> "\t123\tabc" -- a non-integer eval id
  ]

-- | Ported from Hydra's @Hydra\/Event\/EvalStarted.t@.
badTwoFieldPayloads :: [Text]
badTwoFieldPayloads =
  [ "", -- an empty payload
    hydraTraceId, -- one field
    hydraTraceId <> "\t123\t456", -- three fields
    hydraTraceId <> "\tabc" -- a non-integer jobset id
  ]

